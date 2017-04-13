      SUBROUTINE ISLAND (NPKMAX, NX, NY, IMAGE, CUTT, PKWIN, NPK)
C-----------------------------------------------------------------------
C! finds connect regions of emmision (adapted from SAD)
C# Map Modeling Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2009, 2015
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
C   Find points in the residual map whose values are greater than CUTT.
C   All such points which touch each other, either along grid lines or
C   diagonals, are considered to be one "island".  Give each distinct
C   island its own number and compute a rectangular window that just
C   contains that island.
C   Inputs
C      NPKMAX   I      Maximum allowed number of islands
C      NX       I      X dimension
C      NY       I      Y dimension
C      IMAGE    R(*,*) image itself
C   Outputs:
C      PKWIN    R(4,NPKMAX)   A list of minX,minY,maxX,maxY window
C                      corners for islands
C      NPK      I      Number of islands found
C   Routines adapted from SAD which retains its own versions of them
C-----------------------------------------------------------------------
      INTEGER   NPKMAX, NX, NY, PKWIN(4,*), NPK
      REAL      CUTT, IMAGE(NX,NY)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   OLD(MAXIMG), NEW(MAXIMG), I, J, LOCWIN(4)
      LOGICAL   DIAG
      SAVE OLD, NEW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NPK = 0
      LOCWIN(1) = 1
      LOCWIN(2) = 1
      LOCWIN(3) = NX
      LOCWIN(4) = NY
      DIAG = NPKMAX.GT.0
C                                       Initialize previous ine
      CALL FILL (NX, 0, OLD)
C
      DO 200 J = 1,NY
         DO 100 I = 1,NX
C                                       Is point above cutoff?
            IF ((IMAGE(I,J).EQ.FBLANK) .OR. (IMAGE(I,J).LT.CUTT)) THEN
               NEW(I) = 0
C                                       Are any adjacent points, on
C                                       currnt Line or previous line
C                                       already marked?
            ELSE IF ((I.GT.1) .AND. (NEW(I-1).GT.0)) THEN
               NEW(I) = NEW(I-1)
C                                       link to previous now?
               IF ((DIAG) .AND. (I.LT.NX) .AND. (OLD(I+1).GT.0))
     *            CALL MERGPK (NEW(I), OLD(I+1), LOCWIN, NPK, PKWIN,
     *            OLD, NEW)
            ELSE IF ((DIAG) .AND. (I.GT.1) .AND. (OLD(I-1).NE.0)) THEN
               NEW(I) = OLD(I-1)
C                                       Is this A link between two
C                                       previous distinct islands?
               IF ((I.LT.NX) .AND. (OLD(I+1).GT.0)) CALL MERGPK (NEW(I),
     *            OLD(I+1), LOCWIN, NPK, PKWIN, OLD, NEW)
            ELSE IF (OLD(I).NE.0) THEN
               NEW(I) = OLD(I)
            ELSE IF ((DIAG) .AND. (I.LT.NX) .AND. (OLD(I+1).NE.0)) THEN
               NEW(I) = OLD(I+1)
C                                       Totally new island
C                                       skip ADDPK
            ELSE IF (NPK.LT.ABS(NPKMAX)) THEN
               NEW(I) = NPK + 1
               NPK  = NEW(I)
               PKWIN(1,NPK) = I
               PKWIN(2,NPK) = J
               PKWIN(3,NPK) = I
               PKWIN(4,NPK) = J
               GO TO 100
            ELSE
               NEW(I) = 0
               END IF
C                                       New addition to old island
            IF (NEW(I).NE.0) CALL ADDPK (PKWIN, I, J, NEW(I))
 100        CONTINUE
C                                       Get ready for next line.
         CALL COPY (NX, NEW(LOCWIN(1)), OLD(LOCWIN(1)))
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ADDPK (PKWIN, I, J, IPK)
C-----------------------------------------------------------------------
C   Add a single point with coordinates I,J to an existing island by
C   adjusting the boundaries of the Island
C-----------------------------------------------------------------------
      INTEGER PKWIN(4,*), I, J, IPK
C-----------------------------------------------------------------------
      PKWIN(1,IPK) = MIN (PKWIN(1,IPK), I)
      PKWIN(3,IPK) = MAX (PKWIN(3,IPK), I)
      PKWIN(4,IPK) = J
C
 999  RETURN
      END
      SUBROUTINE MERGPK (IPK, JPK, WIN, NPK, PKWIN, OLD, NEW)
C-----------------------------------------------------------------------
C   Merge two Islands together by finding max boundaries
C   Inputs:
C      IPK     I        first island number
C      JPK     I        second island number
C      WIN     I(4)     full blc,trc window
C   In/Out:
C      NPK     I        Maximum island number - reduced if = higher
C      PKWIN   I(4,*)   windows for islands - higher set to zero
C      OLD     I(*)     window number for previous row - higher set to
C                       lower
C      NEW     I(*)     window number for current row - higher set to
C                       lower
C-----------------------------------------------------------------------
      INTEGER   IPK, JPK, WIN(4), NPK, PKWIN(4,*), OLD(*), NEW(*)
C
      INTEGER   I, IL, IH
C-----------------------------------------------------------------------
      IF (IPK.NE.JPK) THEN
         IL = MIN (IPK, JPK)
         IH = MAX (IPK, JPK)
C                                       get extrema in lower # island
         PKWIN(1,IL) = MIN (PKWIN(1,IPK), PKWIN(1,JPK))
         PKWIN(2,IL) = MIN (PKWIN(2,IPK), PKWIN(2,JPK))
         PKWIN(3,IL) = MAX (PKWIN(3,IPK), PKWIN(3,JPK))
         PKWIN(4,IL) = MAX (PKWIN(4,IPK), PKWIN(4,JPK))
C                                       null upper number island
         PKWIN(1,IH) = 0
         PKWIN(2,IH) = 0
         PKWIN(3,IH) = 0
         PKWIN(4,IH) = 0
C                                       drop upper island
         IF (NPK.EQ.IH) NPK = NPK - 1
         DO 20 I = WIN(1),WIN(3)
            IF (OLD(I).EQ.IH) OLD(I) = IL
            IF (NEW(I).EQ.IH) NEW(I) = IL
 20         CONTINUE
         END IF
C
 999  RETURN
      END
