      SUBROUTINE CALRES (JERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine to calculate the residuals of an image.
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C Calculate residuals of image. Be careful to do correlation step
C properly for optical images
C   Programmer =  T.J. Cornwell      January 1991
C-----------------------------------------------------------------------
      INTEGER    JERR, IFIELD
      REAL       CHISAV, VAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                        Find residuals of current guess
      CHISQ = 0.0
      DO 100 IFIELD = 1,NFIELD
         VAL = 1 / RVAR(IFIELD)
         CHISAV = CHISQ
C                                        Focal or aperture plane data ?
         IF (BEAMTP(IFIELD).GT.0) THEN
C                                        Primary beam correction ?
         IF (BMSIZE(IFIELD).GT.0) THEN
            CALL APPLPB(IFIELD, IMG, PRS, JERR)
            IF (JERR.NE.0) GO TO 990
            CALL CONV (F, PRS, VAL, WT(IFIELD), PRS, JERR)
            IF (JERR.NE.0) GO TO 990
            CALL RESID (IFIELD, PRS, PRS, JERR)
            IF (JERR.NE.0) GO TO 990
            IF (IFIELD.EQ.1) THEN
               CALL APPLPB(IFIELD, PRS, RES, JERR)
               IF (JERR.NE.0) GO TO 990
            ELSE
               CALL APPLPB (IFIELD, PRS, PRS, JERR)
               IF (JERR.NE.0) GO TO 990
               CALL ADDMAP (PRS, RES, RES, 0.0, RESMAX, RESMIN, JERR)
               IF (JERR.NE.0) GO TO 990
               END IF
         ELSE
            CALL CONV (F, IMG, VAL, WT(IFIELD), PRS, JERR)
            IF (JERR.NE.0) GO TO 990
            IF (IFIELD.EQ.1) THEN
               CALL RESID (IFIELD, PRS, RES, JERR)
               IF (JERR.NE.0) GO TO 990
            ELSE
               CALL RESID (IFIELD, PRS, PRS, JERR)
               IF (JERR.NE.0) GO TO 990
               CALL ADDMAP (PRS, RES, RES, 0.0, RESMAX, RESMIN, JERR)
               IF (JERR.NE.0) GO TO 990
               END IF
            END IF
         ELSE
C                                                Focal plane data
            CALL CONV (T, IMG, VAL, WT(IFIELD), PRS, JERR)
            IF (JERR.NE.0) GO TO 990
            CALL RESID (IFIELD, PRS, PRS, JERR)
            IF (JERR.NE.0) GO TO 990
            IF (IFIELD.EQ.1) THEN
               CALL CORR (T, PRS, VAL, WT(IFIELD), RES, JERR)
               IF (JERR.NE.0) GO TO 990
            ELSE
               CALL CORR (T, PRS, VAL, WT(IFIELD), PRS, JERR)
               IF (JERR.NE.0) GO TO 990
               CALL ADDMAP (PRS, RES, RES, 0.0, RESMAX, RESMIN, JERR)
               IF (JERR.NE.0) GO TO 990
               END IF
            END IF
C                                                Save fit for this
C                                                field
         FFIT(IFIELD) = SQRT((CHISQ-CHISAV)/RWNXY)
  100    CONTINUE
C                                                Save overall fit
      FITRMS = SQRT(CHISQ/(NFIELD*RWNXY))
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CALRES')
      END
