# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

#' Pipe operator
#'
#' @author Benjamin Brede
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#'
#' @keywords internal
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @usage lhs \%>\% rhs
#'
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#'
#' @return The result of calling `rhs(lhs)`.
#'
#' @note The pipe operator is imported from the magrittr package and re-exported by this package.
#'       Users should refer to the magrittr documentation for more advanced usage.

NULL
