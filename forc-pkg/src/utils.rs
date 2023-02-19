use std::{
    collections::hash_map,
    fs::{self, File},
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
};

use crate::{
    git_ref_to_refspecs, manifest::MemberManifestFiles, DepKind, Edge, GitReference,
    GitSourceIndex, PackageManifestFile, Pinned, SourceGit,
};
use anyhow::{anyhow, bail, Context, Result};
use forc_util::{git_checkouts_directory, user_forc_directory};
use sway_core::language::parsed::TreeType;
use url::Url;

/// Represents the Head's commit hash and time (in seconds) from epoch
pub(crate) type HeadWithTime = (String, i64);

/// Checks if the toolchain version is in compliance with minimum implied by `manifest`.
///
/// If the `manifest` is a ManifestFile::Workspace, check all members of the workspace for version
/// validation. Otherwise only the given package is checked.
pub(crate) fn validate_version(member_manifests: &MemberManifestFiles) -> Result<()> {
    for member_pkg_manifest in member_manifests.values() {
        member_pkg_manifest.manifest().validate()?;
    }
    Ok(())
}

/// Check if given git commit hash is valid.
pub(crate) fn validate_git_commit_hash(commit_hash: &str) -> Result<()> {
    const LEN: usize = 40;
    if commit_hash.len() != LEN {
        bail!(
            "invalid hash length: expected {}, found {}",
            LEN,
            commit_hash.len()
        );
    }
    if !commit_hash.chars().all(|c| c.is_ascii_alphanumeric()) {
        bail!("hash contains one or more non-ascii-alphanumeric characters");
    }
    Ok(())
}

/// Produce a unique ID for a particular fetch pass.
///
/// This is used in the temporary git directory and allows for avoiding contention over the git
/// repo directory.
pub fn fetch_id(path: &Path, timestamp: std::time::Instant) -> u64 {
    let mut hasher = hash_map::DefaultHasher::new();
    path.hash(&mut hasher);
    timestamp.hash(&mut hasher);
    hasher.finish()
}

/// The name to use for a package's git repository under the user's forc directory.
fn git_repo_dir_name(name: &str, repo: &Url) -> String {
    let repo_url_hash = hash_url(repo);
    format!("{name}-{repo_url_hash:x}")
}

/// The hash of the given url.
fn hash_url(url: &Url) -> u64 {
    let mut hasher = hash_map::DefaultHasher::new();
    url.hash(&mut hasher);
    hasher.finish()
}

/// A temporary directory that we can use for cloning a git-sourced package's repo and discovering
/// the current HEAD for the given git reference.
///
/// The resulting directory is:
///
/// ```ignore
/// $HOME/.forc/git/checkouts/tmp/<fetch_id>-name-<repo_url_hash>
/// ```
///
/// A unique `fetch_id` may be specified to avoid contention over the git repo directory in the
/// case that multiple processes or threads may be building different projects that may require
/// fetching the same dependency.
pub(crate) fn tmp_git_repo_dir(fetch_id: u64, name: &str, repo: &Url) -> PathBuf {
    let repo_dir_name = format!("{:x}-{}", fetch_id, git_repo_dir_name(name, repo));
    git_checkouts_directory().join("tmp").join(repo_dir_name)
}

/// The path to which a git package commit should be checked out.
///
/// The resulting directory is:
///
/// ```ignore
/// $HOME/.forc/git/checkouts/name-<repo_url_hash>/<commit_hash>
/// ```
///
/// where `<repo_url_hash>` is a hash of the source repository URL.
pub fn git_commit_path(name: &str, repo: &Url, commit_hash: &str) -> PathBuf {
    let repo_dir_name = git_repo_dir_name(name, repo);
    git_checkouts_directory()
        .join(repo_dir_name)
        .join(commit_hash)
}

/// Create an advisory lock over the given path.
///
/// See [fd_lock_path] for details.
pub(crate) fn path_lock(path: &Path) -> Result<fd_lock::RwLock<File>> {
    let lock_path = fd_lock_path(path);
    let lock_dir = lock_path
        .parent()
        .expect("lock path has no parent directory");
    std::fs::create_dir_all(lock_dir).context("failed to create forc advisory lock directory")?;
    let lock_file = File::create(&lock_path).context("failed to create advisory lock file")?;
    Ok(fd_lock::RwLock::new(lock_file))
}

/// Given a path to a directory we wish to lock, produce a path for an associated lock file.
///
/// Note that the lock file itself is simply a placeholder for co-ordinating access. As a result,
/// we want to create the lock file if it doesn't exist, but we can never reliably remove it
/// without risking invalidation of an existing lock. As a result, we use a dedicated, hidden
/// directory with a lock file named after the checkout path.
///
/// Note: This has nothing to do with `Forc.lock` files, rather this is about fd locks for
/// coordinating access to particular paths (e.g. git checkout directories).
fn fd_lock_path(path: &Path) -> PathBuf {
    const LOCKS_DIR_NAME: &str = ".locks";
    const LOCK_EXT: &str = "forc-lock";

    // Hash the path to produce a file-system friendly lock file name.
    // Append the file stem for improved readability.
    let mut hasher = hash_map::DefaultHasher::default();
    path.hash(&mut hasher);
    let hash = hasher.finish();
    let file_name = match path.file_stem().and_then(|s| s.to_str()) {
        None => format!("{hash:X}"),
        Some(stem) => format!("{hash:X}-{stem}"),
    };

    user_forc_directory()
        .join(LOCKS_DIR_NAME)
        .join(file_name)
        .with_extension(LOCK_EXT)
}

/// Part of dependency validation, any checks related to the depenency's manifest content.
pub(crate) fn validate_dep_manifest(
    dep: &Pinned,
    dep_manifest: &PackageManifestFile,
    dep_edge: &Edge,
) -> Result<()> {
    let dep_program_type = dep_manifest.program_type()?;
    // Check if the dependency is either a library or a contract declared as a contract dependency
    match (&dep_program_type, &dep_edge.kind) {
        (TreeType::Contract, DepKind::Contract { salt: _ })
        | (TreeType::Library { .. }, DepKind::Library) => {}
        _ => bail!(
            "\"{}\" is declared as a {} dependency, but is actually a {}",
            dep.name,
            dep_edge.kind,
            dep_program_type
        ),
    }
    // Ensure the name matches the manifest project name.
    if dep.name != dep_manifest.project.name {
        bail!(
            "dependency name {:?} must match the manifest project name {:?} \
            unless `package = {:?}` is specified in the dependency declaration",
            dep.name,
            dep_manifest.project.name,
            dep_manifest.project.name,
        );
    }
    dep_manifest.manifest().validate()?;
    Ok(())
}

/// Search and collect repos from checkouts_dir that are from given branch and for the given package
fn collect_local_repos_with_branch(
    checkouts_dir: PathBuf,
    package_name: &str,
    branch_name: &str,
) -> Result<Vec<(PathBuf, HeadWithTime)>> {
    let mut list_of_repos = Vec::new();
    with_search_checkouts(checkouts_dir, package_name, |repo_index, repo_dir_path| {
        // Check if the repo's HEAD commit to verify it is from desired branch
        if let GitReference::Branch(branch) = repo_index.git_reference {
            if branch == branch_name {
                list_of_repos.push((repo_dir_path, repo_index.head_with_time));
            }
        }
        Ok(())
    })?;
    Ok(list_of_repos)
}

/// Search and find the match repo between the given tag and locally available options
fn find_repo_with_tag(
    tag: &str,
    package_name: &str,
    checkouts_dir: PathBuf,
) -> Result<Option<(PathBuf, String)>> {
    let mut found_local_repo = None;
    with_search_checkouts(checkouts_dir, package_name, |repo_index, repo_dir_path| {
        // Get current head of the repo
        let current_head = repo_index.head_with_time.0;
        if let GitReference::Tag(curr_repo_tag) = repo_index.git_reference {
            if curr_repo_tag == tag {
                found_local_repo = Some((repo_dir_path, current_head))
            }
        }
        Ok(())
    })?;
    Ok(found_local_repo)
}

/// Search an exact reference in locally available repos
fn find_exact_local_repo_with_reference(
    checkouts_dir: PathBuf,
    package_name: &str,
    git_reference: &GitReference,
) -> Result<Option<(PathBuf, String)>> {
    let mut found_local_repo = None;
    if let GitReference::Tag(tag) = git_reference {
        found_local_repo = find_repo_with_tag(tag, package_name, checkouts_dir)?;
    } else if let GitReference::Rev(rev) = git_reference {
        found_local_repo = find_repo_with_rev(rev, package_name, checkouts_dir)?;
    }
    Ok(found_local_repo)
}

/// Search and find the match repo between the given rev and locally available options
fn find_repo_with_rev(
    rev: &str,
    package_name: &str,
    checkouts_dir: PathBuf,
) -> Result<Option<(PathBuf, String)>> {
    let mut found_local_repo = None;
    with_search_checkouts(checkouts_dir, package_name, |repo_index, repo_dir_path| {
        // Get current head of the repo
        let current_head = repo_index.head_with_time.0;
        if let GitReference::Rev(curr_repo_rev) = repo_index.git_reference {
            if curr_repo_rev == rev {
                found_local_repo = Some((repo_dir_path, current_head));
            }
        }
        Ok(())
    })?;
    Ok(found_local_repo)
}

/// Search local checkouts directory and apply the given function. This is used for iterating over
/// possible options of a given package.
fn with_search_checkouts<F>(checkouts_dir: PathBuf, package_name: &str, mut f: F) -> Result<()>
where
    F: FnMut(GitSourceIndex, PathBuf) -> Result<()>,
{
    for entry in fs::read_dir(checkouts_dir)? {
        let entry = entry?;
        let folder_name = entry
            .file_name()
            .into_string()
            .map_err(|_| anyhow!("invalid folder name"))?;
        if folder_name.starts_with(package_name) {
            // Search if the dir we are looking starts with the name of our package
            for repo_dir in fs::read_dir(entry.path())? {
                // Iterate over all dirs inside the `name-***` directory and try to open repo from
                // each dirs inside this one
                let repo_dir = repo_dir
                    .map_err(|e| anyhow!("Cannot find local repo at checkouts dir {}", e))?;
                if repo_dir.file_type()?.is_dir() {
                    // Get the path of the current repo
                    let repo_dir_path = repo_dir.path();
                    // Get the index file from the found path
                    if let Ok(index_file) = fs::read_to_string(repo_dir_path.join(".forc_index")) {
                        let index = serde_json::from_str(&index_file)?;
                        f(index, repo_dir_path)?;
                    }
                }
            }
        }
    }
    Ok(())
}

/// Search local checkout dir for git sources, for non-branch git references tries to find the
/// exact match. For branch references, tries to find the most recent repo present locally with the given repo
pub(crate) fn search_git_source_locally(
    name: &str,
    git_source: &SourceGit,
) -> Result<Option<(PathBuf, String)>> {
    // In the checkouts dir iterate over dirs whose name starts with `name`
    let checkouts_dir = git_checkouts_directory();
    match &git_source.reference {
        GitReference::Branch(branch) => {
            // Collect repos from this branch with their HEAD time
            let repos_from_branch = collect_local_repos_with_branch(checkouts_dir, name, branch)?;
            // Get the newest repo by their HEAD commit times
            let newest_branch_repo = repos_from_branch
                .into_iter()
                .max_by_key(|&(_, (_, time))| time)
                .map(|(repo_path, (hash, _))| (repo_path, hash));
            Ok(newest_branch_repo)
        }
        _ => find_exact_local_repo_with_reference(checkouts_dir, name, &git_source.reference),
    }
}

/// Initializes a temporary git repo for the package and fetches only the reference associated with
/// the given source.
pub(crate) fn with_tmp_git_repo<F, O>(
    fetch_id: u64,
    name: &str,
    source: &SourceGit,
    f: F,
) -> Result<O>
where
    F: FnOnce(git2::Repository) -> Result<O>,
{
    // Clear existing temporary directory if it exists.
    let repo_dir = tmp_git_repo_dir(fetch_id, name, &source.repo);
    if repo_dir.exists() {
        let _ = std::fs::remove_dir_all(&repo_dir);
    }

    // Initialise the repository.
    let repo = git2::Repository::init(&repo_dir)
        .map_err(|e| anyhow!("failed to init repo at \"{}\": {}", repo_dir.display(), e))?;

    // Fetch the necessary references.
    let (refspecs, tags) = git_ref_to_refspecs(&source.reference);

    // Fetch the refspecs.
    let mut fetch_opts = git2::FetchOptions::new();
    if tags {
        fetch_opts.download_tags(git2::AutotagOption::All);
    }
    repo.remote_anonymous(source.repo.as_str())?
        .fetch(&refspecs, Some(&mut fetch_opts), None)
        .with_context(|| {
            format!(
                "failed to fetch `{}`. Check your connection or run in `--offline` mode",
                &source.repo
            )
        })?;

    // Call the user function.
    let output = f(repo)?;

    // Clean up the temporary directory.
    let _ = std::fs::remove_dir_all(&repo_dir);
    Ok(output)
}
