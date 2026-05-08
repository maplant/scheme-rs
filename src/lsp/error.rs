use lsp_server::{ExtractError, Notification, ProtocolError};

#[derive(Debug)]
pub enum LspError {
    Protocol(ProtocolError),
    SerializeCapabilities(serde_json::Error),
    ExtractNotification(ExtractError<Notification>),
    PublishDiagnostics,
    JoinIoThreads,
}

impl std::fmt::Display for LspError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Protocol(err) => write!(f, "LSP protocol error: {err}"),
            Self::SerializeCapabilities(err) => {
                write!(f, "failed to serialize server capabilities: {err}")
            }
            Self::ExtractNotification(err) => write!(f, "failed to extract notification: {err}"),
            Self::PublishDiagnostics => write!(f, "failed to publish diagnostics"),
            Self::JoinIoThreads => write!(f, "failed to join LSP IO threads"),
        }
    }
}

impl std::error::Error for LspError {}

impl From<ProtocolError> for LspError {
    fn from(value: ProtocolError) -> Self {
        Self::Protocol(value)
    }
}
