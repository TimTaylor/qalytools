.stop <- function (msg, ..., .call = sys.call(-1L))
{
    err <- errorCondition(msg, ..., class = "qalytools_error", call = .call[1L])
    stop(err)
}

.warning <- function (msg, ..., .call = sys.call(-1L))
{
    err <- warningCondition(msg, ..., class = "qalytools_warning", call = .call[1L])
    warning(err)
}

.stop_fancy <- function(msg, ..., .call = sys.call(-1L)) {
    top <- msg[1L]
    top <- strwrap(top, width = 80, exdent = 2)
    top <- paste(top, collapse = "\n")
    if (length(msg) > 1L) {
        info <- msg[-1L]
        info <- strwrap(sprintf("- %s", info), width = 80, indent = 6, exdent = 8)
        info <- paste(info, collapse = "\n")
        top <- paste(top, info, sep = "\n")
    }
    cnd <- errorCondition(top, ..., class = "qalytools_error", call = .call[1L])
    stop(cnd)
}
