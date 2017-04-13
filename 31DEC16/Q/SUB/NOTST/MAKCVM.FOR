      SUBROUTINE MAKCVM (IN, OUT, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: Make image with residuals added.
C# IO-util Map
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
C Make image with residuals added
C   Programmer =  R. Braun      November 1987
C-----------------------------------------------------------------------
      INTEGER    IN, OUT, IFIELD
      REAL       VAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      DO 100 IFIELD = 1,NFIELD
         IF (FRES(IFIELD).GT.0.0) THEN
            VAL = 1/FRES(IFIELD)**2
         ELSE
            VAL = 1.0
            END IF
C                                        Focal or aperture plane data ?
         IF (BEAMTP(IFIELD).GT.0) THEN
C                                        Primary beam correction ?
            IF (BMSIZE(IFIELD).GT.0) THEN
               CALL APPLPB (IFIELD, IN, PRS, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL CONV (F, PRS, 1.0, WT(IFIELD), PRS, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL SUBMAP (DAT(IFIELD), PRS, PRS, 0.0, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (IFIELD.EQ.1) THEN
                  CALL APLPBI (IFIELD, PRS, VAL, OUT, RES, IERR)
                  IF (IERR.NE.0) GO TO 990
               ELSE
                  VMSZ(1,WK1) = HNX
                  VMSZ(2,WK1) = HNY
                  VMSZ(1,WK2) = HNX
                  VMSZ(2,WK2) = HNY
                  CALL APLPBI (IFIELD, PRS, VAL, WK1, WK2, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL ADDMAP (WK1, OUT, OUT, FBLANK, RESMAX, RESMIN,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL ADDMAP (WK2, RES, RES, FBLANK, RESMAX, RESMIN,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  VMSZ(1,WK1) = NX
                  VMSZ(2,WK1) = NY
                  VMSZ(1,WK2) = NX
                  VMSZ(2,WK2) = NY
                  END IF
            ELSE
               CALL CONV (F, IN, 1.0, WT(IFIELD), PRS, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL SUBMAP (DAT(IFIELD), PRS, PRS, 0.0, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (IFIELD.EQ.1) THEN
                  CALL MLTMAP (PRS, VAL, OUT, 0.0, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL FLAT (VAL, RES, IERR)
                  IF (IERR.NE.0) GO TO 990
               ELSE
                  VMSZ(1,WK1) = HNX
                  VMSZ(2,WK1) = HNY
                  VMSZ(1,WK2) = HNX
                  VMSZ(2,WK2) = HNY
                  CALL MLTMAP (PRS, VAL, WK1, 0.0, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL FLAT (VAL, WK2, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL ADDMAP (WK1, OUT, OUT, 0.0, RESMAX, RESMIN, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL ADDMAP (WK2, RES, RES, 0.0, RESMAX, RESMIN, IERR)
                  IF (IERR.NE.0) GO TO 990
                  VMSZ(1,WK1) = NX
                  VMSZ(2,WK1) = NY
                  VMSZ(1,WK2) = NX
                  VMSZ(2,WK2) = NY
                  END IF
               END IF
         ELSE
C                                                Focal plane data
            CALL CONV (T, IN, 1.0, WT(IFIELD), PRS, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SUBMAP (DAT(IFIELD), PRS, PRS, 0.0, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (IFIELD.EQ.1) THEN
               CALL MLTMAP (PRS, VAL, OUT, 0.0, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL FLAT (VAL, RES, IERR)
               IF (IERR.NE.0) GO TO 990
            ELSE
               VMSZ(1,WK1) = HNX
               VMSZ(2,WK1) = HNY
               VMSZ(1,WK2) = HNX
               VMSZ(2,WK2) = HNY
               CALL MLTMAP (PRS, VAL, WK1, 0.0, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL ADDMAP (WK1, OUT, OUT, FBLANK, RESMAX, RESMIN, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL FLAT (VAL, WK2, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL ADDMAP (WK2, RES, RES, FBLANK, RESMAX, RESMIN, IERR)
               IF (IERR.NE.0) GO TO 990
               VMSZ(1,WK1) = NX
               VMSZ(2,WK1) = NY
               VMSZ(1,WK2) = NX
               VMSZ(2,WK2) = NY
               END IF
            END IF
  100    CONTINUE
C                                       Now divide by (primary beam)**2
      IF (BLANKD) THEN
         CALL DIVMAP (OUT, RES, RES, FBLANK, RESMAX, RESMIN, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL DIVMAP (OUT, RES, RES, 0.0, RESMAX, RESMIN, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Now make Convolving function
C                                       and convolve with it.
      IFIELD = 1
      CALL MAKGAU (WT(IFIELD), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL CONV (F, IN, 1.0, WT(IFIELD), OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Add residuals after linear
C                                       correction for primary beam.
      IF (BLANKD) THEN
         CALL ADDMAP (OUT, RES, OUT, FBLANK, RESMAX, RESMIN, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL ADDMAP (OUT, RES, OUT, 0.0, RESMAX, RESMIN, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAKCVM')
      END
