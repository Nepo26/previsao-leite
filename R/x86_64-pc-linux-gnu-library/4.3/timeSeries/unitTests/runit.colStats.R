
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################


test.colStats =
function()
{
    # RUnit Test:

    # Signal Series:
    tS = dummyMonthlySeries(format = "counts")
    tS
    colStats(tS, mean)
    colSums(tS)
    colMeans(tS)
    colSds(tS)
    colVars(tS)
    colSkewness(tS)
    colKurtosis(tS)
    colMaxs(tS)
    colMins(tS)
    colProds(tS)
    colQuantiles(tS)

    # timDate Series:
    tS = dummyMonthlySeries()
    tS
    colStats(tS, mean)
    colSums(tS)
    colMeans(tS)
    colSds(tS)
    colVars(tS)
    colSkewness(tS)
    colKurtosis(tS)
    colMaxs(tS)
    colMins(tS)
    colProds(tS)
    colQuantiles(tS)
}


################################################################################

