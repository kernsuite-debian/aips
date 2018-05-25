      SUBROUTINE RUVFIL (NV, NAXIS, DISK, CNO, IILOCB, IERR)
C-----------------------------------------------------------------------
C! FITS 3-D UV table  - header check build or expand file
C# FITS EXT-util tape UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2015
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
C   This routine will check the current header for a UV file from FITS
C   binary UV table data.  If it is a null header, the existing file is
C   deleted, the header corrected and a proper file created.  If the
C   header is okay, the file is checked and expanded if needed,
C   Inputs:
C      NV       I(4)   IPIECE, NPIECE, FIRSTVIS, LASTVIS
C      NAXIS    I(2)   Length of columns (in char), number of rows.
C      DISK     I      Output disk
C      CNO      I      Output slot number
C   Outputs:
C      IILOCB   I      = -1 if no BASELINE _ ANTENNA1/2+SA conversion
C                      else subscript for BASELINE random parameter
C      IERR     I      Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   NV(4), NAXIS(2), DISK, CNO, IILOCB, IERR
C
      INTEGER   I, NP, J, SIZE, ASIZE, LUN, IND, IEX, IBL, COMPIN
      CHARACTER PNAME*48, STR*8
      DOUBLE PRECISION XFREQ, JD0
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DBHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN /16/
C-----------------------------------------------------------------------
      IILOCB = -1
      CALL ZPHFIL ('UV', DISK, CNO, 1, PNAME, IERR)
C                                       check header
      NP = 1
      DO 10 I = 1,CATBLK(KIDIM)
         NP = NP * CATBLK(KINAX+I-1)
 10      CONTINUE
      IF (DATOBS.NE.' ') THEN
         CALL JULDAY (DATOBS, JD0)
      ELSE
         JD0 = 0
         END IF
      XFREQ = 0.0D0
      DO 15 I = 1,CATBLK(KIDIM)
         CALL H2CHR (8, 1, CATH(KHCTP+2*(I-1)), STR)
         IF (STR(:4).EQ.'FREQ') XFREQ = CATD(KDCRV+I-1)
 15      CONTINUE
      IF (XFREQ.LE.0.0D0) THEN
         DO 20 I = 1,MAXIS
            IF (CTYPE(I)(:4).EQ.'FREQ') XFREQ = CRVAL(I)
 20         CONTINUE
         END IF
      IF (XFREQ.LE.0.0D0) XFREQ = 1.0D0
C                                       conversions needed
      COMPIN = -1
      DO 30 I = 1,ITNCOL
C                                       check number format
         IF (TTYPE(I)(:8).EQ.'VISIBILI') THEN
            IF (MAXISI(1).EQ.2) THEN
               J = INDEX (TFORM(I), 'I')
               IF (J.EQ.0) THEN
                  MSGTXT = 'UNEXPECTED FORMAT FOR 2-VALUE VIS'
                  CALL MSGWRT (8)
               ELSE
                  MAXISI(1) = 1
                  END IF
            ELSE IF (MAXISI(1).EQ.3) THEN
               J = INDEX (TFORM(I), 'E')
               IF (J.EQ.0) THEN
                  MSGTXT = 'UNEXPECTED FORMAT FOR 3-VALUE VIS'
                  CALL MSGWRT (8)
                  END IF
               END IF
         ELSE IF (TTYPE(I)(:8).EQ.'BASELINE') THEN
            IILOCB = I
         ELSE IF (TTYPE(I)(:8).EQ.'WEIGHT  ') THEN
            COMPIN = COMPIN + 1
         ELSE IF (TTYPE(I)(:8).EQ.'SCALE   ') THEN
            COMPIN = COMPIN + 1
         ELSE IF (TTYPE(I)(:2).EQ.'UU') THEN
            TTYPE(I)(3:4) = '-L'
            TSCAL(I) = TSCAL(I) * XFREQ
            TZERO(I) = TZERO(I) * XFREQ
         ELSE IF (TTYPE(I)(:2).EQ.'VV') THEN
            TTYPE(I)(3:4) = '-L'
            TSCAL(I) = TSCAL(I) * XFREQ
            TZERO(I) = TZERO(I) * XFREQ
         ELSE IF (TTYPE(I)(:2).EQ.'WW') THEN
            TTYPE(I)(3:4) = '-L'
            TSCAL(I) = TSCAL(I) * XFREQ
            TZERO(I) = TZERO(I) * XFREQ
         ELSE IF (TTYPE(I).EQ.'DATE') THEN
            TTYPE(I) = 'TIME1'
            TZERO(I) = TZERO(I) - JD0
            END IF
 30      CONTINUE
C                                       build from scratch
      IF ((CATBLK(KIGCN).LE.0) .OR. (NP.LE.1)) THEN
         CATBLK(KIDIM) = MAXIS
         NP = 1
         DO 110 I = 1,CATBLK(KIDIM)
            NP = NP * MAXISI(I)
            CATBLK(KINAX+I-1) = MAXISI(I)
            CATR(KRCIC+I-1) = CDELT(I)
            CATR(KRCRP+I-1) = CRPIX(I)
            CATR(KRCRT+I-1) = CROTA(I)
            CATD(KDCRV+I-1) = CRVAL(I)
            CALL CHR2H (8, CTYPE(I), 1, CATH(KHCTP+2*(I-1)))
 110        CONTINUE
         CATD(KDORA) = OBSRA
         CATD(KDODE) = OBSDEC
         CALL CHR2H (8, DATOBS, 1, CATH(KHDOB))
         CALL CHR2H (8, TELESC, 1, CATH(KHTEL))
         CALL CHR2H (8, OBSRVR, 1, CATH(KHOBS))
         CALL CHR2H (8, BUNIT, 1, CATH(KHBUN))
         CALL CHR2H (8, OBJECT, 1, CATH(KHOBJ))
         CATR(KREPO) = CEPOCH
         CATBLK(KIGCN) = MAX (NV(4), NAXIS(2))
         CATBLK(KIPCN) = ITNCOL - 1
         IF (IILOCB.GE.1) CATBLK(KIPCN) = ITNCOL + 1
         J = 0
         DO 120 I = 1,ITNCOL
            IF (TTYPE(I)(:8).EQ.'BASELINE') THEN
               CALL CHR2H (8, 'SUBARRAY', 1, CATH(KHPTP+J))
               J = J + 2
            ELSE IF (TTYPE(I)(:8).NE.'VISIBILI') THEN
               IF ((COMPIN.LE.0) .OR. ((TTYPE(I).NE.'WEIGHT') .AND.
     *            (TTYPE(I).NE.'SCALE'))) THEN
                  CALL CHR2H (8, TTYPE(I), 1, CATH(KHPTP+J))
                  J = J + 2
                  END IF
               END IF
 120        CONTINUE
         IF (IILOCB.GT.0) THEN
            CALL CHR2H (8, 'ANTENNA1', 1, CATH(KHPTP+J))
            J = J + 2
            CALL CHR2H (8, 'ANTENNA2', 1, CATH(KHPTP+J))
            J = J + 2
            END IF
         IF (COMPIN.EQ.1) THEN
            CALL CHR2H (8, 'WEIGHT  ', 1, CATH(KHPTP+J))
            J = J + 2
            CALL CHR2H (8, 'SCALE   ', 1, CATH(KHPTP+J))
            J = J + 2
            END IF
C                                       UV file: destroy old
         CALL ZDESTR (DISK, PNAME, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'DESTROYING DUMMY UV FILE'
            GO TO 980
            END IF
C                                       create corrrect size file
         NP = NP + CATBLK(KIPCN)
         CALL UVSIZE (NP, CATBLK(KIGCN), SIZE)
         CALL ZCREAT (DISK, PNAME, SIZE, .TRUE., ASIZE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CREATING PROPER UV FILE'
            GO TO 980
            END IF
C                                       header is okay: check match
      ELSE
         IF ((MAXIS.GT.0) .AND. (MAXIS.NE.CATBLK(KIDIM))) THEN
            WRITE (MSGTXT,1100) 'DIMENSIONS', MAXIS, CATBLK(KIDIM)
            GO TO 970
            END IF
         NP = 1
         DO 210 I = 2,CATBLK(KIDIM)
            NP = NP * CATBLK(KINAX+I-1)
            IF ((MAXISI(I).GT.0) .AND. (MAXISI(I).NE.CATBLK(KINAX+I-1)))
     *         THEN
               WRITE (MSGTXT,1100) 'NAXISi', MAXISI(I),
     *            CATBLK(KINAX+I-1)
               GO TO 970
               END IF
            CALL H2CHR (8, 1, CATH(KHCTP+2*(I-1)), STR)
            IF (STR.EQ.' ') THEN
               CALL CHR2H (8, CTYPE(I), 1, CATH(KHCTP+2*(I-1)))
            ELSE
               IF (STR.NE.CTYPE(I)(:8)) THEN
                  WRITE (MSGTXT,1101) 'AXIS TYPE', STR, CTYPE(I)
                  GO TO 970
                  END IF
               END IF
            IF ((CDELT(I).NE.0.0) .AND. (CATR(KRCIC+I-1).EQ.0.0))
     *         CATR(KRCIC+I-1) = CDELT(I)
            IF ((CRPIX(I).NE.0.0) .AND. (CATR(KRCRP+I-1).EQ.0.0))
     *         CATR(KRCRP+I-1) = CRPIX(I)
            IF ((CROTA(I).NE.0.0) .AND. (CATR(KRCRT+I-1).EQ.0.0))
     *         CATR(KRCRT+I-1) = CROTA(I)
            IF ((CRVAL(I).NE.0.0D0) .AND. (CATD(KDCRV+I-1).EQ.0.0D0))
     *         CATD(KDCRV+I-1) = CRVAL(I)
 210        CONTINUE
         IEX = 1
         IF ((CATBLK(KINAX).EQ.3) .AND. (MAXISI(1).EQ.1)) IEX = 3
         IBL = 0
         IF (IILOCB.GT.0) IBL = 2
         IF (ITNCOL+IBL.NE.CATBLK(KIPCN)+IEX) THEN
            WRITE (MSGTXT,1100) '# RAND PARM', ITNCOL-1, CATBLK(KIPCN)
            GO TO 970
            END IF
         IF ((MAXISI(1).EQ.1) .OR. (MAXISI(1).EQ.3)) CATBLK(KINAX) =
     *      MAXISI(1)
         NP = NP * CATBLK(KINAX)
         J = 0
         DO 220 I = 1,CATBLK(KIPCN)
            CALL H2CHR (8, 1, CATH(KHPTP+J), STR)
            J = J + 2
            IF (TTYPE(I)(:8).EQ.'BASELINE') THEN
               IF (STR.NE.'SUBARRAY') THEN
                  WRITE (MSGTXT,1101) 'RAND TYPE', STR, 'SUBARRAY'
                  GO TO 970
                  END IF
            ELSE IF ((IILOCB.GT.0) .AND. (STR(:7).EQ.'ANTENNA')) THEN
            ELSE IF ((TTYPE(I)(:8).NE.'VISIBILI') .AND. (I.LE.ITNCOL))
     *         THEN
               IF (STR.NE.TTYPE(I)(:8)) THEN
                  WRITE (MSGTXT,1101) 'RAND TYPE', STR, TTYPE(I)(:8)
                  GO TO 970
                  END IF
               END IF
 220        CONTINUE
         IF (IEX.GT.1) THEN
            J = CATBLK(KIPCN)
            CATBLK(KIPCN) = CATBLK(KIPCN) + 2
            CALL CHR2H (8, TTYPE(J+1), 1, CATH(KHPTP+2*J))
            CALL CHR2H (8, TTYPE(J+2), 1, CATH(KHPTP+2*J+2))
            END IF
C                                       current size
         CALL ZEXIST (DISK, PNAME, ASIZE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'ZEXIST FAILS ON EXISTING FILE'
            GO TO 980
            END IF
         NP = NP + CATBLK(KIPCN)
C                                       concat without "pieces"
         IF (NV(4).LT.NAXIS(2)) THEN
            CATBLK(KIGCN) = NAXIS(2) + CATBLK(KIGCN)
C                                       with pieces info
         ELSE
            CATBLK(KIGCN) = MAX (CATBLK(KIGCN), NV(4))
            END IF
         CALL UVSIZE (NP, CATBLK(KIGCN), SIZE)
         ASIZE = SIZE - ASIZE
C                                       expand
         IF (ASIZE.GT.0) THEN
            CALL ZOPEN (LUN, IND, DISK, PNAME, .TRUE., .TRUE., .TRUE.,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN UV FILE TO EXPAND'
               GO TO 980
               END IF
            CALL ZEXPND (LUN, DISK, PNAME, ASIZE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'EXPANDING UV FILE'
               GO TO 980
               END IF
            CALL ZCLOSE (LUN, IND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE FILE AFTER EXPAND'
               GO TO 980
               END IF
            END IF
         END IF
      GO TO 999
C                                       compare error
 970  IERR = 10
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RUVFIL: ERROR',I3,' ON ',A)
 1100 FORMAT ('RUVFIL: ',A,' MISMATCH',2I8)
 1101 FORMAT ('RUVFIL: ',A,' MISMATCH ''',A,''' ''',A,'''')
      END
