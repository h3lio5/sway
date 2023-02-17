use crate::pkg::{ConstInjectionMap, MemberFilter};
use sway_core::BuildTarget;

#[derive(Default, Clone)]
pub struct PkgOpts {
    /// Path to the project, if not specified, current working directory will be used.
    pub path: Option<String>,
    /// Offline mode, prevents Forc from using the network when managing dependencies.
    /// Meaning it will only try to use previously downloaded dependencies.
    pub offline: bool,
    /// Terse mode. Limited warning and error output.
    pub terse: bool,
    /// Requires that the Forc.lock file is up-to-date. If the lock file is missing, or it
    /// needs to be updated, Forc will exit with an error
    pub locked: bool,
    /// The directory in which the sway compiler output artifacts are placed.
    ///
    /// By default, this is `<project-root>/out`.
    pub output_directory: Option<String>,
}

/// The set of options to control the level of logging.
#[derive(Default, Clone)]
pub struct PrintOpts {
    /// Print the generated Sway AST (Abstract Syntax Tree).
    pub ast: bool,
    /// Print the computed Sway DCA (Dead Code Analysis) graph.
    pub dca_graph: bool,
    /// Print the finalized ASM.
    ///
    /// This is the state of the ASM with registers allocated and optimisations applied.
    pub finalized_asm: bool,
    /// Print the generated ASM.
    ///
    /// This is the state of the ASM prior to performing register allocation and other ASM
    /// optimisations.
    pub intermediate_asm: bool,
    /// Print the generated Sway IR (Intermediate Representation).
    pub ir: bool,
}

/// The set of options to control minification of certain human readable outputs.
#[derive(Default, Clone)]
pub struct MinifyOpts {
    /// By default the JSON for ABIs is formatted for human readability. By using this option JSON
    /// output will be "minified", i.e. all on one line without whitespace.
    pub json_abi: bool,
    /// By default the JSON for initial storage slots is formatted for human readability. By using
    /// this option JSON output will be "minified", i.e. all on one line without whitespace.
    pub json_storage_slots: bool,
}

/// The set of options provided to the `build` functions.
#[derive(Default)]
pub struct BuildOpts {
    pub pkg: PkgOpts,
    pub print: PrintOpts,
    pub minify: MinifyOpts,
    /// If set, outputs a binary file representing the script bytes.
    pub binary_outfile: Option<String>,
    /// If set, outputs source file mapping in JSON format
    pub debug_outfile: Option<String>,
    /// Build target to use.
    pub build_target: BuildTarget,
    /// Name of the build profile to use.
    /// If it is not specified, forc will use debug build profile.
    pub build_profile: Option<String>,
    /// Use release build plan. If a custom release plan is not specified, it is implicitly added to the manifest file.
    ///
    ///  If --build-profile is also provided, forc omits this flag and uses provided build-profile.
    pub release: bool,
    /// Output the time elapsed over each part of the compilation process.
    pub time_phases: bool,
    /// Include all test functions within the build.
    pub tests: bool,
    /// List of constants to inject for each package.
    pub const_inject_map: ConstInjectionMap,
    /// The set of options to filter by member project kind.
    pub member_filter: MemberFilter,
}
