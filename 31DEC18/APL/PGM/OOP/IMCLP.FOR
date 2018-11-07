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
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC',
     *   'PIXRANGE',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT, OOAINT,
     *   OOARE,
     *   OOACAR, OOACAR, OOAINT, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 7,1, 7,1,
     *   2,1,
     *   12,1, 6,1, 1,1, 1,1/
LOCAL END
      PROGRAM IMCLP
C-----------------------------------------------------------------------
C! Clip an image to a specified range.
C# Task IMAGE-UTIL OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2010
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
      REAL      PIXRA(2)
      INTEGER   IRET, BUFF1(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PAOOF.INC'
      DATA PRGM /'IMCLP'/
C                                       Startup
      CALL IMCOIN (PRGM, IN, PIXRA, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Clip
      CALL IMGCLP (IN, PIXRA, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL IMCOHI (IN, OUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE IMCOIN (PRGN, IN, PIXRA, OUT, IRET)
C-----------------------------------------------------------------------
C   IMCOIN gets input parameters for IMCLP and creates the objects.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IN      C*?  Input image object
C      PIXRA   R(2) Range of pixel values desired
C      OUT     C*?  Output image object.
C      IRET    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, IN*(*), OUT*(*)
      REAL      PIXRA(2)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs for IN
      PARAMETER (NKEY1=10)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INAME*12, ONAME*12,
     *   OCLASS*6, CDUM*1
      REAL      RTEMP
      INTEGER   TYPE, DIM(3), IDUM
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get pixel range
      CALL OGET ('Input', 'PIXRANGE', TYPE, DIM, PIXRA, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (PIXRA(1).GT.PIXRA(2)) THEN
C                                       PIXRA=(min,max)
         RTEMP = PIXRA(1)
         PIXRA(1) = PIXRA(2)
         PIXRA(2) = RTEMP
         END IF
C                                       Default outname = INNAME
      CALL OGET ('Input', 'INNAME', TYPE, DIM, IDUM, INAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, IDUM, ONAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ONAME.EQ.'            ') THEN
         ONAME = INAME
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, IDUM, ONAME, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Default OUTCLASS = 'IMCLP'
      CALL OGET ('Input', 'OUTCLASS', TYPE, DIM, IDUM, OCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OCLASS.EQ.'      ') THEN
         OCLASS = 'IMCLP'
         CALL OPUT ('Input', 'OUTCLASS', TYPE, DIM, IDUM, OCLASS,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Create IN
      IN = 'Input image object'
      CALL CREATE (IN, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output object
      OUT = 'Output image'
      CALL IMGCLN (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMCOHI (IN, OUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output.
C   Inputs:
C      IN      C*?  Input object
C      OUT     C*?  Output object
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=7)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'BLC', 'TRC', 'PIXRANGE'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       copy tables
      CALL IMCALT (IN, OUT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUT
      CALL MSGWRT (6)
 999  RETURN
      END
C   Note: the following really should go into the relevant class
C   libraries.  If this is done ARRCLP can be made more efficient by
C   using ARRIO rather than ARRREAD/ARRWRI and working directly in the
C   I/O buffer.
      SUBROUTINE IMGCLP (IN, RANGE, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clips the values in the input image to a specifiec range.  Values
C   out of range are replaced with the closest value in the range.
C   Inputs:
C      IN     C*?   The name of the input image object.
C      RANGE  R(2)  Range of valid values
C      OUT    C*?   The name of the output image object.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      RANGE(2)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Setup OUT if necessary
      CALL IMCLNX (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGOPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Process array
      CALL ARRCLP (IN, RANGE, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMGCLP: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'IMGCLP: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRCLP (IN, RANGE, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clip an array to a specified range.
C   Only handles real (including blanked) arrays.
C   Inputs:
C      IN     C*32  The name of the input array object.
C      RANGE  R(2)  Range of allowed values.
C      OUT    C*32  The name of the output array object.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      RANGE(2)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NAXIS(7), DIM(7), TYPE, DUMMY
      CHARACTER DATYPE*8, TIN*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INTEGER   MAXROW
      PARAMETER (MAXROW=4096)
      REAL      ROW(MAXROW)
      LOGICAL   VALID(MAXROW)
      INCLUDE 'INCS:DMSG.INC'
C-------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN.EQ.OUT) THEN
C                                       Create temporary output object
         TIN = 'Temporary IMAGE for ARRCLP'
         CALL IMGCOP (IN, TIN, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN = IN
         END IF
C                                       Open arrays
      CALL ARROPN (TIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Window
      CALL ARRWIN (TIN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
C                                       Error if Complex
      IF(DOCMPL) THEN
         IERR = 1
         MSGTXT = 'ARRCLP: CANNOT DO COMPLEX ARRAYS'
         GO TO 990
         END IF
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input row
      CALL ARREAD (TIN, DIM, ROW, IERR)

      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
C                                       Clip row with blanking
         CALL CHKBLK (LROW, ROW, VALID)
         CALL RVBCLP (ROW, VALID, RANGE, ROW, VALID, LROW)
         CALL SETBLK (LROW, VALID, ROW)
      ELSE
C                                       Clip row
         CALL RVCLP (ROW, RANGE, ROW, LROW)
         END IF
      CALL ARRWRI (OUT, DIM, ROW, IERR)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN(1:32).NE.IN(1:32)) CALL IMGDES (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ARRCLP: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRCLP: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE RVCLP (IN1, RANGE, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real vector clip
C   Inputs:
C      IN1    R(*)  Input vector
C      RANGE  R(2)  Range of valid values
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), RANGE(2), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP)
         IF (OUT(LOOP).LT.RANGE(1)) OUT(LOOP) = RANGE(1)
         IF (OUT(LOOP).GT.RANGE(2)) OUT(LOOP) = RANGE(2)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBCLP (IN1, VALIN1, RANGE, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real vector clip with blanking
C   Inputs:
C      IN1    R(*)  Input vector
C      VALIN1 L(*)  True if corresponding element of IN1 is valid.
C      RANGE  R(2)  Range of valid values
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), RANGE(2), OUT(N)
      LOGICAL   VALIN1(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN1(LOOP)
         IF (VALOUT(LOOP)) THEN
            OUT(LOOP) =  IN1(LOOP)
            IF (OUT(LOOP).LT.RANGE(1)) OUT(LOOP) = RANGE(1)
            IF (OUT(LOOP).GT.RANGE(2)) OUT(LOOP) = RANGE(2)
            END IF
 100     CONTINUE
C
 999  RETURN
      END
