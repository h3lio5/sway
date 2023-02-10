//! Functionality for accessing context-specific information about the current contract or message.
library context;

use ::registers::balance;

/// Get the amount of units of `call_frames::msg_asset_id()` being sent.
pub fn msg_amount() -> u64 {
    balance()
}
