#!/usr/bin/env python
# Timothy Chagnon
# Sat Mar 14 16:50:26 EDT 2009
#
# Plots performance data from CSV files
# with plot styles copied from spiral.net/bench.html
#
# Requires Matplotlib http://matplotlib.sourceforge.net/

import sys
from pylab import *

# Colors taken from plots on spiral.net/bench.html
SPIRAL_RED = '#cc2127'
LIGHT_GRAY = '#ebeced'

rcParams['xtick.direction'] = 'out'

def makePlot(inputFile):
    A = load(inputFile, delimiter=',')
    X = A[:49,0]
    B = A[:49,1:]
    mean = B.mean(axis=1)
    mins = mean - B.min(axis=1)
    maxs = B.max(axis=1) - mean
    #yerr = B.var(axis=1) 
    yerr = vstack((mins,maxs))

    spiralPlot(X,mean,yerr)

    xlabel('Faces per Bounding Box')
    ylabel('Mean Time (seconds), Variance')
    title('Bounding Box Threshold vs. Ray-tracing time'
        , horizontalalignment='left'
        , position=(-.05, 1.06)
        )

def spiralPlot(X, Y, yerr):
    """All the formatting to make a plot look right."""
    # Gray background, bottom only border
    ax = axes(axisbg=LIGHT_GRAY, axisbelow=True)
    ax.axesFrame.set_data((0,1), (0,0))
    f = ax.get_frame()
    #f.set_clip_on(False)
    f.set_linewidth(0)

    # Make X tick marks for every data point
    #xticks(X)
    ax.xaxis.set_ticks_position('bottom')

    # Make all Y ticks white to be invisible
    for l in ax.yaxis.get_ticklines():
        l.set_color('w')

    # Horizontal grid of white lines
    ax.yaxis.grid(True, linestyle='-', color='white', linewidth=2)

    errorbar(X, Y, yerr=yerr, xerr=None, fmt='o'
        , color = SPIRAL_RED
        #, markeredgecolor = SPIRAL_RED
        #, linewidth = 2.0
        , zorder = 3  # above grid lines
        )

    # Shrink X axis to tight bounds with data plus a little
    xmargin = 1
    xlim( (min(X)-xmargin, max(X)+xmargin) )
    ylim(ymin=0)

def main():
    if len(sys.argv) < 2:
        print 'Usage: %s <input.csv> [<output-image>]' % sys.argv[0] + """
  Arguments:
    <input.DP.txt>  required, CSV data file with rows of x, y1, y2, y3, ...
    <output-image>  optional, output image file.  Extension determines type.
                    Possibilities are eps, jpeg, pdf, png, ps, svg.
                    No argument opens graph in window.
"""
        return

    inputFile = sys.argv[1]
    if len(sys.argv) > 2:
        outFile = sys.argv[2]
    else:
        outFile = None

    makePlot(inputFile)

    if outFile:
        savefig(outFile)
    else:
        show()

if __name__=='__main__':
    main()
