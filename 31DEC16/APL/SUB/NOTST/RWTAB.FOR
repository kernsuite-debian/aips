      SUBROUTINE RWTAB (FDVEC, TBIND, DPTR, NAXIS, IBLK, TAPBUF, IERR)
C-----------------------------------------------------------------------
C! Read FITS ASCII table data and write AIPS table file.
C# FITS EXT-util Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008, 2015
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
C  This routine will read the data section of a FITS extension file
C  of type TABLE and decode this information using data obtained from
C  the header section of the extension file, and write the AIPS version
C  of a table file.  Limited to tables lines <= 2880 characters.
C  Inputs:
C     FDVEC    I(50)      File descriptor vector for TAPIO input
C     DPTR     I(128,2)   Data Pointers, used in table file control.
C     NAXIS    I(2)       Length of columns (in char), number of rows.
C  In/Out:
C     TBIND    I          Buffer pointer in TAPBUF
C     IBLK     I(>=512)     Disk Table file I/O buffer.
C     TAPBUF   I(*)       Tape I/O buffer.
C  Outputs:
C     IERR     I          Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER TLINE*2880
      HOLLERITH RECH(1)
      INTEGER   NAXIS(2), FDVEC(50), TBIND, DPTR(128,2), IBLK(*),
     *   TAPBUF(*), IERR
      DOUBLE PRECISION X, RECRD(1)
      REAL      RECRR(512)
      INTEGER   NAXIS2, LCTR, RECII(1), IT(256), ILEN(256), NCNTR,
     *   I, NAXIS1, IOFF, IT0, JT, JTRIM
      LOGICAL   NULL, RECLL(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTHD.INC'
      EQUIVALENCE (RECII, RECLL, RECRD, RECRR, RECH)
C-----------------------------------------------------------------------
      NCNTR = 2881
C                                       Calculate end & type
C                                       of Column values.
C                                       Use TFCODE to determine types
C                                       because DPTR holds output info.
      DO 10 I = 1,ITNCOL
         IT(I) = MOD (TFCODE(I),10)
C                                       Length of Character types in
C                                       reals.
         IF (IT(I).EQ.3) ILEN(I) = ((DPTR(I,2) / 10) + 3) / 4
 10      CONTINUE
C                                       Loop for all lines in table.
      NAXIS1 = NAXIS(1)
      NAXIS2 = NAXIS(2)
      DO 800 LCTR = 1,NAXIS2
C                                       Read a FITS table data line.
         CALL TABLIN (NAXIS1, FDVEC, TBIND, NCNTR, TAPBUF, TLINE, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 200 I = 1,ITNCOL
C                                       Decode the data fields in this
C                                       row.  Look for null values.
            NULL = TLINE(TBCOL(I):TBCOL(I)+TWIDTH(I)-1) .EQ.
     *         TNULL(I)(1:TWIDTH(I))
C                                       Go to correct type
            IOFF = DPTR(I,1)
            IT0 = IT(I)
C                                       Double precision
            IF (IT0.LE.1) THEN
               IF (.NOT.NULL) THEN
                  CALL DCODEF (TBCOL(I), TWIDTH(I), TFRAC(I), TLINE,
     *               X, IERR)
                  RECRD(IOFF) = X
               ELSE
                  RECRD(IOFF) = DBLANK
                  END IF
C                                       Single precision
            ELSE IF (IT0.EQ.2) THEN
               IF (.NOT.NULL) THEN
                  CALL DCODEF (TBCOL(I), TWIDTH(I), TFRAC(I), TLINE,
     *               X, IERR)
                  RECRR(IOFF) = X
               ELSE
                  RECRR(IOFF) = FBLANK
                  END IF
C                                       Character.
            ELSE IF (IT0.EQ.3) THEN
               IF (.NOT.NULL) THEN
                  JT = JTRIM (TLINE(TBCOL(I):TBCOL(I)+TWIDTH(I)-1))
                  CALL CHR2H (TWIDTH(I), TLINE(TBCOL(I):), 1,
     *               RECH(IOFF))
               ELSE
                  CALL CHFILL (TWIDTH(I), HBLANK, RECH(IOFF))
                  END IF
C                                       Integer
            ELSE IF ((IT0.EQ.4) .OR. (IT0.EQ.6)) THEN
               IF (.NOT.NULL) THEN
                  CALL DCODEF (TBCOL(I), TWIDTH(I), TFRAC(I), TLINE,
     *               X, IERR)
                  IF (X.GT.0.0D0) THEN
                     RECII(IOFF) = X + 0.5
                  ELSE
                     RECII(IOFF) = X - 0.5
                     END IF
               ELSE
                  RECII(IOFF) = BLANKV
                  END IF
               GO TO 200
C                                       Logical
            ELSE IF (IT0.EQ.5) THEN
               RECLL(IOFF) = TLINE(TBCOL(I):TBCOL(I)).EQ.'T'
               GO TO 200
C                                       Hexbit
C                                       Not supported.
            ELSE
               END IF
C
 200        CONTINUE
C                                       Write this table row.
         CALL TABIO ('WRIT', 0, LCTR, RECRR, IBLK, IERR)
 800     CONTINUE
      CALL TABIO ('CLOS', 0, LCTR, RECRR, IBLK, IERR)
C
 999  RETURN
      END
