LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=11)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'IN2SEQ',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'BLC', 'TRC'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT, OOAINT, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1, 7,1, 7,1/
LOCAL END
      PROGRAM BSAVG
C-----------------------------------------------------------------------
C! Weighted averaging of beam-switched images
C# Task OOP SINGLEDISH
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2005, 2012
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
      CHARACTER PRGM*6, IN*32, OUT*32
      INTEGER   IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'BSAVG '/
C-----------------------------------------------------------------------
C                                       Startup & HI
      CALL BSAVIN (PRGM, IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate
      CALL BSAVDO (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BSAVIN (PRGN, IN, OUT, IRET)
C-----------------------------------------------------------------------
C   BSAVIN gets input parameters for BSAVG, creates the output and even
C   copies the history.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      IN       C*?    Input object
C      OUT      C*?    Output object
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, IN*(*), OUT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NADV
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=10)
      PARAMETER (NADV=7)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, CNAME*8, KEYW*8,
     *   LIST(NADV)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
C                    1          2        3        4        5      6
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                                       Rename
C                    1       2       3        4       5      6
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'IN2SEQ',
     *   'BLC', 'TRC'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Declare 'PARANGLE' a header
C                                       keyword for the image class.
C                                       Also TOTHROW as header word
      CNAME = 'IMAGE'
      KEYW = 'PARANGLE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
      KEYW = 'TOTHROW'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create IN
      IN = 'Input image object'
      CALL CREATE (IN, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
      CALL OOPEN (IN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clone output from IN2
      OUT = 'Output interpolated image'
      CALL CREATE (OUT, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clone
      CALL IMGCLN (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create
      CALL OOPEN (OUT, 'DEST', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy beam parameters
      CALL BEMCOP (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Copy old history
      CALL OHCOPY (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, OUT, IRET)
C
 999  RETURN
      END
      SUBROUTINE BSAVDO (IN, OUT, IRET)
C-----------------------------------------------------------------------
C   BSAVDO does the FFT averaging
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      IN       C*?    Input object
C      OUT      C*?    Output object
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER IN*(*), OUT*(*)
C
      CHARACTER FFT*32, PADD*32, CDUMMY*1, DATYPE*8
      INTEGER   BLC(7), TRC(7), INAX(7), NX, NY, IN1SEQ, IN2SEQ, INSEQ,
     *   TYPE, DIM(7), BLCI(7), TRCI(7), NAXISI(7), DUMMY, BLCF(7),
     *   TRCF(7), NXF(7), NWORD, NFX, NFY
      LONGINT   SPTR, WPTR
      REAL      FACTR, SUMS(2), PARANG, TTHROW, XSIZE, YSIZE, XT, YT,
     *   CDELT(7)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL OGET ('Input', 'INSEQ', TYPE, DIM, IN1SEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'IN2SEQ', TYPE, DIM, IN2SEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get size and window in original
C                                       Image
      CALL IMGWIN (IN, BLCI, TRCI, NAXISI, IRET)
      IF (IRET.NE.0) GO TO 999
C
      PADD = 'Copy & pad image'
      FFT = 'FFT of padded image'
C                                       Loop over seq numbers
      DO 100 INSEQ = IN1SEQ,IN2SEQ
C                                       Force seq number
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (IN, 'IMSEQ', OOAINT, DIM, INSEQ, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Input image, copy to
C                                       scratch file with zero padding
C                                       for FFT.
         CALL FFTPAD (IN, PADD, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get IN parameters
         CALL OGET (IN, 'PARANGLE', TYPE, DIM, PARANG, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET (IN, 'TOTHROW', TYPE, DIM, TTHROW, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL IMDGET (IN, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get size of FFT for
C                                       normalization
         CALL IMGWIN (PADD, BLC, TRC, INAX, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       save first values
         IF (INSEQ.EQ.IN1SEQ) THEN
            NX = INAX(1)
            NY = INAX(2)
            XSIZE = INAX(1) * ABS(CDELT(1)) * 3600.
            YSIZE = INAX(2) * ABS(CDELT(2)) * 3600.
C                                       check later
         ELSE
            XT = INAX(1) * ABS(CDELT(1)) * 3600.
            YT = INAX(2) * ABS(CDELT(2)) * 3600.
            IF ((NX.NE.INAX(1)) .OR. (NY.NE.INAX(2)) .OR.
     *         (ABS(XT-XSIZE).GT.0.001*XSIZE) .OR.
     *         (ABS(YT-XSIZE).GT.0.001*YSIZE)) THEN
               WRITE (MSGTXT,1000) INSEQ, IN1SEQ
               CALL MSGWRT (8)
               IRET = 9
               GO TO 999
               END IF
            END IF
C                                       Make copy of file for FFT
C                                       output.
         MSGTXT = 'pad'
         CALL MSGWRT (1)
         CALL IMGCOP (PADD, FFT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Default datatype = COMPLEX
         DIM(1) = 8
         DIM(2) = 1
         DIM(3) = 0
         DATYPE = 'COMPLEX'
         CALL IMPUT (FFT, 'ARRAY.ARRAY_DESC.DATATYPE', OOACAR, DIM,
     *      DUMMY, DATYPE, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Forward FFT
         MSGTXT = 'fft'
         CALL MSGWRT (1)
         CALL IMGFFT (3, PADD, FFT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Allocate and clear sum arrays
         IF (INSEQ.EQ.IN1SEQ) THEN
            MSGTXT = 'allocate memory'
            CALL MSGWRT (1)
            CALL IMGWIN (FFT, BLCF, TRCF, NXF, IRET)
            IF (IRET.NE.0) GO TO 999
            NFX = NXF(1) / 2
            NFY = NXF(2)
            NWORD = (3 * NFX * NFY - 1) / 1024 + 2
            CALL ZMEMRY ('GET', 'BSAVDO', NWORD, SUMS, SPTR, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'UNABLE TO GET MEMORY ALLOCATION'
               CALL MSGWRT (7)
               GO TO 999
               END IF
            NWORD = 1024 * NWORD
            WPTR = SPTR + 2 * NFX * NFY
            CALL RFILL (NWORD, 0.0, SUMS(1+SPTR))
            END IF
         MSGTXT = 'add'
         CALL MSGWRT (1)
C                                       need more parms to do right
         CALL BSWTIM ('SUM', FFT, NFX, NFY, PARANG, TTHROW, XSIZE,
     *      YSIZE, SUMS(1+SPTR), SUMS(1+WPTR), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Close in to allow open new one
         CALL IMGCLO (IN, IRET)
         IF (IRET.NE.0) GO TO 999
 100     CONTINUE
C                                       divide by weights
      CALL BSWTIM ('DIV', FFT, NFX, NFY, PARANG, TTHROW, XSIZE, YSIZE,
     *   SUMS(1+SPTR), SUMS(1+WPTR), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       FFT back
      MSGTXT = 'back fft'
      CALL MSGWRT (1)
      CALL IMGFFT (-1, FFT, PADD, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get window in intermediate
C                                       image.
      CALL COPY (7, BLCI, BLC)
      CALL COPY (7, TRCI, TRC)
      BLC(1) = NX/2 + 1 - NAXISI(1)/2
      TRC(1) = BLC(1) + NAXISI(1) - 1
      BLC(2) = NY/2 + 1 - NAXISI(2)/2
      TRC(2) = BLC(2) + NAXISI(2) - 1
      DIM(1) = 7
      DIM(2) = 1
      DIM(3) = 0
      CALL IMPUT (PADD, 'ARRAY.ARRAY_DESC.BLC', OOAINT, DIM, BLC,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMPUT (PADD, 'ARRAY.ARRAY_DESC.TRC', OOAINT, DIM, TRC,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       subimage to original field
      FACTR = 1.0
      CALL IMGSCL (PADD, FACTR, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Delete temporary objects and
C                                       scratch files.
      CALL IMGZAP (PADD, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMGDES (FFT, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZMEMRY ('FRAL', 'BSAVDO', NWORD, SUMS, SPTR, NWORD)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSAVDO: SEQUENCE NUMBER',I5,' SIZE DOES NOT MATCH',I5)
      END
      SUBROUTINE BSWTIM (OP, FIL, NX, NY, PANG, THROW, XS, YS, SUM, WTS,
     *   IRET)
C-----------------------------------------------------------------------
C   Weight image according to its bs weighting
C   Inputs:
C      OP     C*?    'SUM' -> add FIL to SUM, WTS, 'DIV' write SUM/WTS
C      FIL    C*?    Input image to weight, output image averaged
C      NX     I      Number X pixels
C      NY     I      Number Y pixels
C      PANG   R      Parallactic angle in degrees
C      THROW  R      Total throw in arc sec
C      XS     R      X field of view in arc sec
C      YS     R      Y field of view in arc sec
C   In/out:
C      SUM    R(*)   Current sum of wt*data
C      WTS    R(*)   Current sum of weights
C   Outputs:
C      IRET   I      Error return; 0 okay
C-----------------------------------------------------------------------
      CHARACTER OP*(*), FIL*(*)
      INTEGER   NX, NY, IRET
      REAL      PANG, THROW, XS, YS, SUM(2,NX,NY), WTS(NX,NY)
C
      INTEGER   I, J, DIM(7), I2
      REAL      W, ROW(2,4096), FJ, FI
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Sum it
      IF (OP.EQ.'SUM') THEN
         FJ = PI * THROW * COS (DG2RAD*PANG) / XS
         FI = PI * THROW * SIN (DG2RAD*PANG) / YS
         I2 = NX / 2
         CALL ARROPN (FIL, 'READ', IRET)
         IF (IRET.NE.0) GO TO 999
         DO 20 J = 1,NY
            CALL ARREAD (FIL, DIM, ROW, IRET)
            IF (IRET.NE.0) GO TO 999
            W = 1.0
            DO 10 I = 1,NX
               IF (I.LE.I2) THEN
                  W = SIN ((J-1)*FJ - (I-1)*FI)
               ELSE
                  W = SIN ((J-1)*FJ - (NX+1-I)*FI)
                  END IF
               IF ((I.EQ.1) .AND. (J.EQ.1)) W = 1.0
               W = W * W
               SUM(1,I,J) = SUM(1,I,J) + W * ROW(1,I)
               SUM(2,I,J) = SUM(2,I,J) + W * ROW(2,I)
               WTS(I,J) = WTS(I,J) + W
 10            CONTINUE
 20         CONTINUE
C                                       average it
      ELSE IF (OP(:3).EQ.'DIV') THEN
         CALL ARROPN (FIL, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         DO 120 J = 1,NY
            DO 110 I = 1,NX
               IF (WTS(I,J).EQ.0.0) THEN
                  ROW(1,I) = 0.0
                  ROW(2,I) = 0.0
               ELSE
                  ROW(1,I) = SUM(1,I,J) / WTS(I,J)
                  ROW(2,I) = SUM(2,I,J) / WTS(I,J)
                  END IF
 110           CONTINUE
            CALL ARRWRI (FIL, DIM, ROW, IRET)
            IF (IRET.NE.0) GO TO 999
 120        CONTINUE
         END IF
C
      CALL ARRCLO (FIL, IRET)
C
 999  RETURN
      END
