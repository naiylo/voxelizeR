% SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
% SPDX-FileCopyrightText: 2024 Benjamin Brede
% SPDX-License-Identifier: GPL-3.0-or-later

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_equal.R
\name{split_equal}
\alias{split_equal}
\title{Split vector into equal parts}
\usage{
split_equal(x, n)
}
\arguments{
\item{x}{vector to split}

\item{n}{number of groups to split x into}
}
\value{
list with parts of the vector
}
\description{
This function divides a vector x into n groups of equal length, or the vector in
groups of length l If n is 1, it returns the original vector. Otherwise, it uses
the cut function to create a factor that splits the sequence along the vector into
n groups, which is then used by the split function to divide the vector accordingly.
}
\examples{
# Example 1: Split vector into 3 groups of roughly equal length
split_equal(1:10, 3)

# Example 2: Split vector into 5 groups of roughly equal length
split_equal(1:10, 5)

# Example 3: Attempt to split vector into 20 groups (some will be empty)
split_equal(1:10, 20)

# Example 4: Split an empty vector into 3 groups (returns a list of 3 empty vectors)
split_equal(integer(0), 3)

}
\author{
Benjamin Brede
}
