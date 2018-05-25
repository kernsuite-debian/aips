      SUBROUTINE PLEND (ISTAT, IDEBUG)
C-----------------------------------------------------------------------
C! End-of-plot clean-up functions: Gary plot package.
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   Do some plotting cleanup functions.  Write "end of plot" record,
C   close plot file, check for vectors that were off the plot.
C   Then terminates the task with a call to DIE.
C   Inputs:
C     ISTAT  I         0=successful completion, other=dies unnaturally.
C     IDEBUG I         > 0 => don't delete PL file despite errors
C-----------------------------------------------------------------------
      INTEGER   ISTAT, IDEBUG
C
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVD.INC'
C-----------------------------------------------------------------------
C                                       Was plot even opened?
      IF ((GPHIND.LE.0) .AND. (TVIND.LE.0)) GO TO 980
C                                       vectors off plot message
      IF (IOFFPL.GT.0) THEN
         WRITE (MSGTXT,1000) IOFFPL
         CALL MSGWRT (6)
         END IF
C                                       Attempt close partial graph
 10   IF (ISTAT.GT.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (6)
         END IF
C                                       Close plot file
 20   CALL GFINIS (PLTBLK, IERR)
C                                       OK finish - do HI file
      IF (.NOT.DOTV) THEN
         IF ((IERR.EQ.0) .OR. (IDEBUG.GT.0)) THEN
            CALL HIPLOT (IMSTUF(7), IMSTUF(5), IVER, PLTBLK, IERR)
            WRITE (MSGTXT,1020) IVER
            CALL MSGWRT (2)
C                                       Finish not sucessful. Destroy.
         ELSE
            CALL ZCLOSE (GPHLUN, GPHIND, IERR)
            CALL ZDESTR (GPHVOL, GPHNAM, IERR)
            CALL DELEXT ('PL', IMSTUF(7), IMSTUF(5), 'RDRD', CATBLK,
     *         IOBLK, IVER, IERR)
            END IF
         END IF
C                                       Close down task
 980  CALL DIE (ISTAT, PLTBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I7,' VECTORS TRUNCATED AT EDGES OF PLOT.')
 1010 FORMAT ('ERROR DURING GRAPHING WILL TRY TO FINISH PARTIAL GRAPH')
 1020 FORMAT ('Plot file version',I4,' created')
      END
