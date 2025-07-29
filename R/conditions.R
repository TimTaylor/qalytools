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
