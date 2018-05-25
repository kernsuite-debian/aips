C Program CLFIX
C Last mods 24 May 1999, D. Gordon, GSFC
C******************************************************************************
C
LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=10)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
C
LOCAL INCLUDE 'INPUTDATA.INC'
C                                     DATA statments defining input parameters.
C                                       NOTE: Uses values in PAOOF.INC
C                                       Adverb names
C                      1        2         3           4        5
      DATA AVNAME /'USERID', 'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           6          7         8         9       10
     *   'INVERS', 'OUTVERS', 'INFILE', 'APARM', 'DPARM' /
C                                       Adverb data types (PAOOF.INC)
C                     1       2      3       4        5       6
      DATA AVTYPE /OOAINT, OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C           7       8      9     10
     *   OOAINT, OOACAR, OOARE, OOARE /
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5    6    7     8     9    10
      DATA AVDIM /1,1, 12,1, 6,1, 1,1, 1,1, 1,1, 1,1, 48,1, 10,1, 10,1/
LOCAL END
      PROGRAM CLFIX
C-----------------------------------------------------------------------
C! Add IM and MC table quantities to CL table
C# Task VLBI Calibration Utility OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 1999
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
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Stuff clock offsets and atmospheres from MC table into CL table, and
C   recompute model delay coefficients from IM table at CL table time
C   and replace in CL table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, UVDATA*36, OUTTAB*36
      INTEGER  IRET, BUFF1(256)
      DATA PRGM /'CLFIX '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL CLFXIN (PRGM, INTAB, UVDATA, OUTTAB, IRET)
      IF (IRET.NE.0)  GO TO 990
C                                       Process table
      CALL CLPROC (INTAB, UVDATA, OUTTAB, IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
 999  STOP
      END
C
      SUBROUTINE CLFXIN (PRGN, INTAB, UVDATA, OUTTAB, IRET)
C-----------------------------------------------------------------------
C   CLFXIN gets input parameters for CLFIX and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      INTAB   C*   Name of input CL table object.
C      UVDATA  C*   Name of input uv data
C      OUTTAB  C*   Name of output table object.
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C
C   Inputs from INTAB:
C
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C
      INTEGER  IRET
      CHARACTER PRGN*6, INTAB*36, UVDATA*36, OUTTAB*36
      REAL      APARM(10), DPARM(10)
      COMMON /APRM/ APARM, DPARM
      INTEGER   DIM(3), TYPE, VER2, DUMMY, FDUM(16), DIMN(3)
C
      INTEGER DISKIN, SEQIN, OLDCNO, NUSER, LUN
      CHARACTER NAMEIN*12, CLAIN*6
      COMMON /IMSTUF/ DISKIN, SEQIN, OLDCNO, NUSER, LUN, NAMEIN, CLAIN
C
      INTEGER   NKEY1, NKEY2, NKEY3
C                                       NKEY1=no. adverbs to copy to INTAB
      PARAMETER (NKEY1=9)
C                                       NKEY2=no. adverbs to copy to UVDATA
      PARAMETER (NKEY2=6)
C                                       NKEY3=no. adverbs to copy to OUTTAB
      PARAMETER (NKEY3=6)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *          OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32,
     *          CDUMMY*1, CCDUM*1
C                                       Adverbs to copy to INTAB
C                    1         2          3        4         5
      DATA INK1 /'USERID', 'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           6         7         8        9
     *   'INVERS', 'INFILE', 'APARM', 'DPARM'/
C                                       May rename adverbs to INTAB
C                    1         2       3         4      5       6
      DATA OUTK1 /'USERID', 'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
C           7        8        9
     *   'IFILE', 'APARM', 'DPARM'/
C
C                                       Adverbs to copy to OUTTAB
C                    1         2          3        4         5
      DATA INK2 /'USERID', 'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           6
     *   'OUTVERS'/
C                                       May rename adverbs to OUTTAB
C                    1         2       3        4      5       6
      DATA OUTK2 /'USERID', 'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER'/
C
C                                       Adverbs to copy to UVDATA
C                    1        2        3       4         5       6
      DATA INK3 /'USERID','INNAME','INCLASS','INSEQ','INDISK','INVERS'/
C                                       May rename adverbs to UVDATA
C                    1         2       3         4      5       6
      DATA OUTK3 /'USERID', 'NAME', 'CLASS', 'IMSEQ', 'DISK', 'CLUSE'/
C
C-----------------------------------------------------------------------
C                     Startup, returns "Input" object containing POPS adverbs
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C
C  Set input CL table version number
      CALL OGET ('Input', 'INVERS', TYPE, DIM, VER2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input object
      INTAB = 'Input table'
      CALL CREATE (INTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, INTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (INTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CL', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object
      OUTTAB = 'Output table'
      CALL CREATE (OUTTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUTTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      DIM(1) = 2
      CALL OPUT (OUTTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CL', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create UV data OBJECT
      UVDATA = 'Input uv data'
      CALL CREATE (UVDATA, 'UVDATA', IRET)
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C
      DIM(1) = 1
C                                       Get APARM array
      CALL OGET (INTAB, 'APARM', TYPE, DIMN, APARM, CCDUM, IRET)
      IF(IRET.NE.0) GO TO 999
C                                       Get DPARM array
      CALL OGET (INTAB, 'DPARM', TYPE, DIMN, DPARM, CCDUM, IRET)
      IF(IRET.NE.0) GO TO 999
C       Set defaults
       IF (DPARM(1).LT.0) DPARM(1) = 0
       IF (DPARM(1).GT.2) DPARM(1) = 0
C
C   Get stuff so we can read the IM and MC tables.
      CALL OGET (INTAB, 'NAME',  TYPE, DIM, FDUM, NAMEIN, IRET)
      CALL OGET (INTAB, 'CLASS', TYPE, DIM, FDUM, CLAIN,  IRET)
      CALL OGET (INTAB, 'IMSEQ', TYPE, DIM, SEQIN, CCDUM, IRET)
      CALL OGET (INTAB, 'DISK',  TYPE, DIM, DISKIN, CCDUM, IRET)
      CALL OGET (INTAB, 'USERID',  TYPE, DIM, NUSER, CCDUM, IRET)
C
 999  RETURN
      END
C-------------------------------------------------------------------------
      BLOCK DATA CLFILE
C
      DOUBLE PRECISION IMTIM(3000)
      REAL IMTINT
      INTEGER IMSRC(3000), IMSUB(3000), IMROW(3000,20), IMKNT
     *        ,MCROW(3000,20)
      COMMON /IMMAP/ IMTIM, IMSRC, IMSUB, IMROW, MCROW, IMTINT, IMKNT
C
      DATA IMTIM /3000*0.D0/
      DATA IMSRC /3000*0/
      DATA IMSUB /3000*0/
      DATA IMROW /60000*0/
      DATA MCROW /60000*0/
C
      END
C-----------------------------------------------------------------------
      SUBROUTINE CLPROC (INTAB, UVDATA, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Inputs:
C      INTAB   C*?   Name of input table object.
C      UVDATA  C*?   Name of input uv data
C      OUTTAB  C*?   Name of output table object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
      CHARACTER INTAB*(*), UVDATA*(*), OUTTAB*(*)
      INTEGER   TYPE, DIM(3), ANTMAX, NPOLN, NROW, CURROW, NTERMS,
     *   NUMIF, IERR
C     INTEGER J, K
      REAL GMMOD
      CHARACTER  CDUMMY*1
      REAL      APARM(10), DPARM(10)
      COMMON /APRM/ APARM, DPARM
      DOUBLE PRECISION IMTIM(3000)
      REAL IMTINT
      INTEGER IMSRC(3000), IMSUB(3000), IMROW(3000,20), IMKNT
     *        ,MCROW(3000,20)
      COMMON /IMMAP/ IMTIM, IMSRC, IMSUB, IMROW, MCROW, IMTINT, IMKNT
      INTEGER DISKIN, SEQIN, OLDCNO, NUSER, LUN
      CHARACTER NAMEIN*12, CLAIN*6, UTYPE*2, STAT*4
      REAL BUFF2(26112)
      COMMON /IMSTUF/ DISKIN, SEQIN, OLDCNO, NUSER, LUN, NAMEIN, CLAIN
C-----------------------------------------------------------------------
C
C  Open input CL table
      CALL OCLINI (INTAB, 'READ', CURROW, ANTMAX, NPOLN, NUMIF, NTERMS,
     *   GMMOD, IERR)
      IF (IERR.NE.0) GO TO 999
C  Get number of CL table entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
C  Get stuff for the IM table
        OLDCNO = 1
        UTYPE = 'UV'
        CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *            UTYPE, NUSER, STAT, BUFF2, IERR)
C       PRINT *, ' CATDIR: IERR = ', IERR
C   Get CATBLK
        CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF2, IERR)
C
C   Create/open output table
      CALL OCLINI (OUTTAB, 'WRIT', CURROW, ANTMAX, NPOLN, NUMIF, NTERMS,
     *   GMMOD, IERR)
      IF (IERR.NE.0) THEN
       PRINT *, 'Output table error = ', IERR
       GO TO 999
      ENDIF
C
C***********************************************************************
C       OPEN(UNIT=10, FILE='/tmp/test.climmc',
C    *       STATUS='UNKNOWN',ACCESS='APPEND')
C***********************************************************************
C
C   Read through IM table
       CALL IMSORT (IERR)
C
C   Read thru MC table for clock offsets and atmosphere corrections
       IF (DPARM(1) .LE. 1) THEN
         CALL MCSORT (UVDATA, IERR)
       ENDIF
C
C************************************************************************
C        DO 701 J = 1, IMKNT
C         WRITE(10, 710) J, IMTIM(J), IMSRC(J), (IMROW(J,K), K=1,12),
C    *                   (MCROW(J,K), K=1,12)
C701     CONTINUE
C710      FORMAT (I6,F9.6,I3,12I6,/,18X,12I6)
C710      FORMAT (I6,F9.6,I3,20I6,/,18X,20I6)
C        CLOSE (10)
C************************************************************************
C
C  Read thru CL table
      CALL CLGET (INTAB, OUTTAB, NROW, NTERMS, NPOLN, ANTMAX, NUMIF,
     *            IERR)
C
C   Close tables
 800  CONTINUE
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (OUTTAB, IERR)
C     IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
C
C-------------------------------------------------------------------------
      SUBROUTINE IMSORT (IERR)
C-----------------------------------------------------------------------
C   Read through the IM table and save row numbers for matching with the
C    CL table.
C
C   Output:
C      IERR    I    Return code, -1 => data all processed, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
      INTEGER NROW, IERR, SOURID, ANTNO, SUBA, K, JROW, NUM
C     DOUBLE PRECISION GEODLY(9)
      REAL TIMEI
C
      DOUBLE PRECISION IMTIM(3000)
      REAL IMTINT
      INTEGER IMSRC(3000), IMSUB(3000), IMROW(3000,20), IMKNT
     *        ,MCROW(3000,20)
      COMMON /IMMAP/ IMTIM, IMSRC, IMSUB, IMROW, MCROW, IMTINT, IMKNT
C
      INTEGER BUFFIM(1024)
      INTEGER NTERM, INVER, IRET
      INTEGER DISKIN, SEQIN, OLDCNO, NUSER, LUN
      CHARACTER NAMEIN*12, CLAIN*6
      COMMON /IMSTUF/ DISKIN, SEQIN, OLDCNO, NUSER, LUN, NAMEIN, CLAIN
C-----------------------------------------------------------------------
       IMKNT = 0
       NUM = 0
       NTERM = 6
       INVER = 1
       LUN = 38
C  Initialize IM table
       CALL IMINI ('READ', BUFFIM, DISKIN, OLDCNO, INVER, CATBLK, LUN,
     *      IRET)
        NROW = BUFFIM(5)
C
      DO 1000 JROW = 1, NROW
C    Read data for this row
           IIMRNO = JROW
         CALL IMTAB ('READ', BUFFIM, NOBAND, IRET )
          IF(IRET.NE.0) THEN
           PRINT *, ' IMSORT/IMTAB: IRET, JROW ', IRET, JROW
           GO TO 997
          ENDIF
           TIMEI = TINT
           SOURID = ISRC
           ANTNO = NOSTA
           SUBA = IARRAY
C   Check for valid source
          IF (SOURID .LE. 0) GO TO 1000
C
C   Determine if this is a new time, new source, new subarray
      IF (IMKNT .NE. 0) THEN
       DO 23 K = 1,IMKNT
C       See if this is a new or an old scan
        IF ( (DABS(TIME - IMTIM(K)) .LT. 1.D-12) .AND.
     *      (SOURID .EQ. IMSRC(K)) )  THEN
C        Old scan, add info to the correct line in IMMAP common block
          IMROW(K,ANTNO) = JROW
C          Set negative if Subarray numbers don't match!
          IF (SUBA .NE. IMSUB(K) ) IMROW(K,ANTNO) = -IMROW(K,ANTNO)
          NUM = NUM + 1
          GO TO 1000
        ENDIF
 23    CONTINUE
      ENDIF
C
C   If we get here, this must be a new scan (new time/source).
            IMKNT = IMKNT + 1
            NUM = NUM + 1
            IMTIM(IMKNT) = TIME
            IMSRC(IMKNT) = SOURID
            IMSUB(IMKNT) = SUBA
            IMROW(IMKNT,ANTNO) = JROW
            IF (IMKNT .EQ. 1) IMTINT = TIMEI
C
 1000  CONTINUE
C
  997  CONTINUE
        CALL TABIO('CLOS',1,IIMRNO,BUFFIM,BUFFIM,IRET)
C
 999  RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE MCSORT (UVDATA, IERR)
C-----------------------------------------------------------------------
C   Read through the MC table. Match MC records with a corresponding IM
C    record (same time, source, antenna number. If there is no match it
C    should not be needed (???), so we move on to the next MC record.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PAOOF.INC'
C     INCLUDE 'INCS:DMSG.INC'
C     INCLUDE 'INCS:DCAT.INC'
      INTEGER   TBKOLS(128,MAXIO), TBNUMV(128,MAXIO)
      COMMON /TBSTUF/ TBKOLS, TBNUMV
C     INTEGER MCCOLS(21)
C     EQUIVALENCE ( TBKOLS(1,1),MCCOLS(1) )
C
      CHARACTER UVDATA*(*)
      INTEGER NROW, IERR, IRNO, RESULT(512), J, K, SRC, IANT, I1, M
      REAL    RES4(512)
      DOUBLE PRECISION RES8(256), TIME1, TDIF
      EQUIVALENCE (RESULT, RES4, RES8)
      DOUBLE PRECISION IMTIM(3000)
      REAL IMTINT
      INTEGER IMSRC(3000), IMSUB(3000), IMROW(3000,20), IMKNT
     *        ,MCROW(3000,20)
      COMMON /IMMAP/ IMTIM, IMSRC, IMSUB, IMROW, MCROW, IMTINT, IMKNT
      INTEGER BUFFIM(512)
      INTEGER INVER, IRET
      INTEGER DISKIN, SEQIN, OLDCNO, NUSER, LUN
      CHARACTER NAMEIN*12, CLAIN*6
      COMMON /IMSTUF/ DISKIN, SEQIN, OLDCNO, NUSER, LUN, NAMEIN, CLAIN
      COMMON /I1MC/I1
C
      CHARACTER TABLE*32,  OBSCD*8, RDATE*8, TAPER*8
      INTEGER   MCROE, NSTOKE, NUMIF, NCHAN, NUMPOL, FFTSIZ,
     *          OVRSMP, ZEROPD
      REAL      RFREQ, CHANBW, REFPX
C
      DOUBLE PRECISION ATMO, DATMO, KLOK, DKLOK
C
C-----------------------------------------------------------------------
C     Values of interest in the MC table:
C       Source number  =  RESULT(?)
C       Antenna number =  RESULT(?)
C         Position of source and antenna not constant
C       Table Time     =  RES8(1)
C       Atmosphere     =  RES8(2)
C       Atmos. Rate    =  RES8(3)
C       Clock Offset   =  RES8(6)
C       Clock Rate     =  RES8(7)
C-----------------------------------------------------------------------
       INVER = 1
       I1 = 0
C  Initialize MC table
       LUN = 37
       CALL TABINI ('READ', 'MC', DISKIN, OLDCNO, INVER, CATBLK, LUN,
     *      NKEY, NREC, NCOL, DATP, BUFFIM, IRET)
       IF(IRET .NE. 0) THEN
        PRINT *, ' MCSORT/TABINI error = ', IRET
        RETURN
       ENDIF
C                          !! Undefined
       NROW = BUFFIM(5)
C       PRINT *, ' NROW = ', NROW
C##    CALL TABIO('CLOS',1,IRNO,BUFFIM,BUFFIM,IRET)
C
C
C##    TABLE = 'MC table'
C##    CALL UV2TAB (UVDATA,TABLE,'MC',1,IERR)
C##     IF(IERR .NE. 0) THEN
C##      print *, ' MCSORT/UV2TAB error = ', IERR
C##      RETURN
C##     ENDIF
C##   CALL OMCINI (TABLE, 'READ', MCROE, OBSCD, RDATE, NSTOKE,
C##  *   STOKE1, NUMIF,  NCHAN, RFREQ, CHANBW, REFPX, NUMPOL, FFTSIZ,
C##  *   OVRSMP, ZEROPD, TAPER, IERR)
C##     IF(IERR .NE. 0) THEN
C##      print *, ' MCSORT/OMCINI error = ', IERR
C##     ENDIF
C
C##    write(6,453)  ( TBKOLS(J,1), J=1,128)
C453   format(/,'TBKOLS:',/, 11(12I10,/))
C
C****************************************************
C      WRITE (10,604) BUFFIM
C604   FORMAT (//,52(10I8,/),//)
C      I1 = BUFFIM(8) - 4
C****************************************************
C
C
       DO 30 J = 1, NROW
C#     DO 30 J = 1, 0
        IRNO = J
        CALL TABIO ('READ', 0, IRNO, RESULT, BUFFIM, IRET)
       IF(IRET .NE. 0) RETURN
C
C#     print *,'J, RESULT ', J, RESULT
C#     print *,'J, RES8   ', J, RES8
C
C  Find location of source and antenna in RESULT array
C    There must be a formula for this, but its too obscure to figure out.
         IF (J.EQ.1) THEN
          DO 35 M = 22,50
            IF( (RESULT(M) .GT. 0) .AND. (RESULT(M) .LE. 200)
     *        .AND. (RESULT(M+1).GE.0) .AND. (RESULT(M+1).LE.20) ) THEN
             I1 = M
             PRINT *, ' I1 = ', I1
             GO TO 36
            ENDIF
  35      CONTINUE
          IF(I1 .EQ. 0) THEN
           PRINT *,' Unable to find station and source in MC table '
           RETURN
          ENDIF
         ENDIF
C
  36    CONTINUE
C
        TIME1 = RES8(1)
        SRC   = RESULT(I1)
        IANT  = RESULT(I1+1)
        ATMO   = RES8(2)
        DATMO  = RES8(3)
        KLOK   = RES8(6)
        DKLOK  = RES8(7)
C
C       SRC  = RESULT(15)
C       IANT = RESULT(48)
C       SRC  = RESULT(33)
C       SRC  = RESULT(25)
C       IANT = RESULT(34)
C       IANT = RESULT(26)
C
C#      TIME1 = RES8(TBKOLS(1,1))
C#      SRC  = RESULT(TBKOLS(2,1))
C#      IANT = RESULT(TBKOLS(3,1))
C#      ATMO   = RES8(TBKOLS(6,1))
C#      DATMO  = RES8(TBKOLS(7,1))
C#      KLOK   = RES8(TBKOLS(10,1))
C#      DKLOK  = RES8(TBKOLS(11,1))
C
C*********************************
C       IF (J .LE. 20)
C    *   print *,'Time,Src,Ant,Atm,Klok', TIME1, SRC, IANT, ATMO,
C    *                                    DATMO, KLOK, DKLOK
C
C    *   print *, 'J, Time, Src, Iant ', J, TIME1, SRC, IANT
C       WRITE(10,601) J, RESULT(33),RESULT(34),RES8(1),RES8(2),RES8(6)
C       WRITE(10,602) J, (RESULT(K), K=17,50)
C601    FORMAT(3I5,F10.7,2E18.10)
C602    FORMAT(I4,2X,34(I3,1X))
C*********************************
C
C  Search through IM table array for a match.
         DO 200 K = 1, IMKNT
          TDIF = TIME1 - IMTIM(K)
          IF ( (DABS(TDIF) .LE. 1.E-9) .AND. (SRC .EQ. IMSRC(K)) ) THEN
C    Set slot number equal to the MC table record number
            MCROW(K,IANT) = J
C           IF (IMROW(K,IANT) .EQ. 0) WRITE(6,1042) TIME1, SRC, IANT
 1042        FORMAT('MC/IM table mismatch (Time, source, antenna): ',
     *              F10.6,2I3)
           GO TO 30
          ENDIF
  200    CONTINUE
C
   30  CONTINUE
C
  997  CONTINUE
C
        CALL TABIO('CLOS',1,IRNO,BUFFIM,BUFFIM,IRET)
C
 999  RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE CLGET (INTAB, OUTTAB, NROW, NTERMS, NPOLN, ANTMAX,
     *                  NUMIF, IERR)
C-----------------------------------------------------------------------
C   Read through the CL table and add clocks, atmospheres, and delay
C    polynomial coefficientss from the MC and IM tables
C
C   Inputs:
C      INTAB   C*?  Input CL table object, should be open.
C      OUTTAB  C*?  Output CL table object, should be open.
C      NROW    I    Total number of rows in input CL table.
C      NTERMS  I    Number of terms in delay polynomial (6).
C      NPOLN   I    Number of polarizations (1 or 2)
C
C   Input/output:
C      ANTMAX  I    Maximum antenna number
C   Output:
C
C      IERR    I    Return code, -1 => data all processed, 0=>OK,
C                   else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER NROW, NTERMS, NPOLN, NUMIF, ANTMAX, IERR, NTERM, KROW
      DOUBLE PRECISION  GEODLY(9), DIF, RSTRT, SAVDIF
      REAL  TIMEI, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      INTEGER SOURCL, CLANT, SUBA, FREQID, REFA(2,MAXIF),
     *    K, JROW, IROW, ISTR, IMREC, MCREC, JREC, J, KK, JK, MCSAVE,
     *    SAVEJ
      DOUBLE PRECISION CTIME, GTIME, MTIME, GDELA(6), ATMO, DATMO, KLOK,
     *   DKLOK, XDIF, A0, A1, A2, A3, A4, A5, T1, T2, T3, T4, T5, TM1
      REAL DIFMIN
      REAL  APARM(10), DPARM(10)
       DOUBLE PRECISION GEODL2(9), DIFF6(6), XX, TMPTIM
       REAL ATMO2, DATMO2, CLOC2(2), DCLOC2(2), DIFF4(4)
       INTEGER IPRNT
      COMMON /APRM/ APARM, DPARM
C
      DOUBLE PRECISION IMTIM(3000)
      REAL IMTINT
      INTEGER IMSRC(3000), IMSUB(3000), IMROW(3000,20), IMKNT
     *        ,MCROW(3000,20)
      COMMON /IMMAP/ IMTIM, IMSRC, IMSUB, IMROW, MCROW, IMTINT, IMKNT
C
C-----------------------------------------------------------------------
       NTERM = 6
       KROW = 0
C      OPEN(UNIT=10, FILE='/tmp/test.clfix', STATUS='UNKNOWN',
C    *      ACCESS='APPEND')
C
      DO 1000 JROW = 1, NROW
C Read CL data for this row
         IROW = JROW
         CALL OTABCL (INTAB, 'READ', IROW, NPOLN, NUMIF, CTIME ,
     *      TIMEI, SOURCL, CLANT, SUBA, FREQID, IFR, GEODLY, DOPOFF,
     *      ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP, CREAL,
     *      CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C
C     Test stuff
        DO J=1,6
         GEODL2(J) = 0.0D0
          DIFF6(J) = 0.0D0
        END DO
        DO J=1,4
          DIFF4(J) = 0.0D0
        END DO
          ATMO2 = 0.0
         DATMO2 = 0.0
          CLOC2(1) = 0.0
          CLOC2(2) = 0.0
         DCLOC2(1) = 0.0
         DCLOC2(2) = 0.0
C
C  Find corresponding IM and MC table records
C
C   IM time MUST be at the two-minute tick BEFORE the CL time. MC time
C    should be the same, but it's not as critical (i.e. we can interpolate).
        ISTR  = CTIME  * 720
        RSTRT = ISTR / 720.D0
C
        SAVDIF = 999.D0
        SAVEJ = 0
        JREC = 0
        MCREC = 0
C
        DO 60 J = 1, IMKNT
          DIF = DABS (IMTIM(J) - RSTRT)
              IF(DIF.LT.SAVDIF) THEN
                SAVDIF = DIF
                SAVEJ  = J
              ENDIF
          IF ( (DIF .LE. 1.D-10) .AND. (SOURCL .EQ. IMSRC(J)) ) THEN
C   Found correct time/source row number in IMROW (and MCROW) array
            JREC = J
            GO TO 100
          ENDIF
  60    CONTINUE
C
C  No match found if we get here
C    Should we proceed or quit?
C            -Quit-
         WRITE(6,1010) CTIME, SOURCL
C#       IF(DPARM(1) .EQ. 2.) GO TO 310
C#       IF(GEODLY(1) .EQ. 0.D0 .AND. GEODLY(2) .EQ. 0.D0) GO TO 310
C#       GO TO 300
         IF(DPARM(1) .EQ. 2.) GO TO 300
         IF(GEODLY(1) .EQ. 0.D0 .AND. GEODLY(2) .EQ. 0.D0) GO TO 300
C            -Proceed-
           JREC = SAVEJ
           GO TO 140
C
 1010    FORMAT('!! No IM time for (time, source) !! ',F10.6,I3)
C
 100    CONTINUE
C*******************************************
         IF (DPARM(1).EQ.1. .OR. DPARM(1).EQ.2.) THEN
C  If redoing delay model coefficients from IM table:
          IMREC = ABS (IMROW(JREC,CLANT))
          IF (IMREC .GT. 0) THEN
C       read IM record
            CALL GETIM(IMREC, RSTRT, CLANT, SOURCL, GTIME, GDELA )
C  Correct the roundoff error in the IM time (error found by Dan Lebach)
C   99MAY24
C                                                Save original time
             TMPTIM = GTIME
C                                                Fix time at the 2-minute mark
             GTIME = DNINT(GTIME*720.D0)/720.D0
C                                                Save difference (seconds) for
C                                                later examination
             TMPTIM = (GTIME-TMPTIM)*86400.D0
C
C  Shift coefficients to CL table time
C    Simplify first:
            A0 = GDELA(1)
            A1 = GDELA(2)
C              skip if these are zero
C???         IF(A0 .EQ. 0.D0 .AND. A1. EQ. 0.D0) GO TO 140
            A2 = GDELA(3)
            A3 = GDELA(4)
            A4 = GDELA(5)
            A5 = GDELA(6)
            T1 = (CTIME - GTIME) * 86400.D0
            T2 = T1**2
            T3 = T1**3
            T4 = T1**4
            T5 = T1**5
C      WRITE (6,1020) (GEODLY(K), K=1,6)
 1020  FORMAT('Before: ',6E15.8)
C    Now shift coeficients:
            GEODL2(1) = A0 + A1*T1 + A2*T2 + A3*T3 + A4*T4 + A5*T5
            GEODL2(2) = A1 + 2.D0*A2*T1 + 3.D0*A3*T2 + 4.D0*A4*T3 +
     *                  5.D0*A5*T4
            GEODL2(3) = A2 + 3.D0*A3*T1 + 6.D0*A4*T2 + 10.D0*A5*T3
            GEODL2(4) = A3 +4.D0*A4*T1 + 10.D0*A5*T2
            GEODL2(5) = A4 + 5.D0*A5*T1
            GEODL2(6) = A5
C      WRITE (6,1021) (GEODL2(K), K=1,6)
 1021  FORMAT('After:  ',6E15.8)
C
C   More test stuff
          IPRNT = 0
          XX = 1.0D-12
        DO J=1,6
         DIFF6(J) = DABS(GEODLY(J) - GEODL2(J))
          XX = XX/10.D0
         IF (DIFF6(J) .GE. XX) IPRNT = IPRNT+1
        END DO
C
          ENDIF
         ENDIF
C
  140 CONTINUE
C*******************************************
         IF (DPARM(1).EQ.0. .OR. DPARM(1).EQ.1.) THEN
C  First double check that we have a source match
           IF(SOURCL .EQ. IMSRC(JREC)) THEN
             MCREC = ABS (MCROW(JREC,CLANT))
           ELSE
             MCREC = 0
           ENDIF
C
           IF (MCREC .GT. 0) THEN
C    Easy case
             CALL GTMC(MCREC, CLANT, SOURCL, MTIME, ATMO, DATMO,
     *            KLOK, DKLOK)
             GO TO 200
           ENDIF
C
           IF (MCREC .EQ. 0) THEN
C    Not so easy case, no match at optimum time
C
             K = 0
             KK = 0
  150        CONTINUE
             K = K + 1
             JK = K
             DO 160 J = 1, 2
               JK = -JK
C
C   Make sure we're not beyond the table bounds
               IF((JREC+JK) .LT. 1 .OR. (JREC+JK) .GT. IMKNT) THEN
                IF((JREC-JK) .LT. 1 .OR. (JREC-JK) .GT. IMKNT) THEN
                 IF(KK.NE.0) THEN
                   MCREC = MCSAVE
                   CALL GTMC(MCREC,CLANT,SOURCL, MTIME,ATMO,DATMO,
     *                  KLOK,DKLOK)
                   ATMO = 0.0D0
                   DATMO = 0.0D0
C             print *,' MC table - beyond match! '
                   GO TO 200
                 ELSE
                   GO TO 300
                 ENDIF
                ENDIF
                GO TO 160
               ENDIF
C
C   Antenna match earlier or later
               IF (MCROW((JREC+JK),CLANT) .NE. 0) THEN
                IF (SOURCL .EQ. IMSRC(JREC+JK)) THEN
C   Source match also
                   MCREC = ABS(MCROW((JREC+JK),CLANT))
                   CALL GTMC(MCREC,CLANT,SOURCL, MTIME,ATMO,DATMO,
     *                  KLOK,DKLOK)
                   XDIF = DABS (MTIME - IMTIM(JREC))
                   DIFMIN = XDIF * 1440.
                  IF(DIFMIN .GT. 8.) THEN
C    Probably a different scan - don't interpolate atmosphere
                    ATMO = 0.0D0
                    DATMO = 0.0D0
                  ENDIF
                   GO TO 200
                ELSE
C   Source mismatch, save first occurrance in case we never get a match
                   KK = KK+1
                  IF (KK.EQ.1) THEN
                    MCSAVE = ABS(MCROW((JREC+JK),CLANT))
                  ENDIF
                ENDIF
               ENDIF
C
  160        CONTINUE
C
                 XDIF = DABS (IMTIM(JREC+JK) - IMTIM(JREC))
                 DIFMIN = XDIF * 1440.
C   If getting far from CL time, stop looking once we have a clock offset
                 IF(DIFMIN .GT. 20. .AND. KK.GE.1) THEN
                  MCREC = MCSAVE
                  CALL GTMC(MCREC,CLANT,SOURCL, MTIME,ATMO,DATMO,
     *                       KLOK,DKLOK)
                  ATMO = 0.0D0
                  DATMO = 0.0D0
                  GO TO 200
                 ENDIF
             GO TO 150
C
           ENDIF
C
         ENDIF
C
  200   CONTINUE
C   Interpolate clocks and atms to CL table time
C    Time shift, in seconds
           TM1 = (CTIME  - MTIME) * 86400.D0
            ATMO2    = ATMO + TM1*DATMO
           DATMO2    = DATMO
            CLOC2(1) = KLOK + TM1 * DKLOK
           DCLOC2(1) = DKLOK
C        WRITE(6,1022) CTIME, MTIME, ATMO, DATMO, KLOK, DKLOK
C        WRITE(6,1023) TM1, ATMO2, DATMO2, CLOC2(1), DCLOC2(1)
 1022    FORMAT('CTIME, etc ',2F10.5,4E18.8)
 1023    FORMAT('TM1, etc  ', F20.5,4E18.8)
C
 300     CONTINUE
C
C  Even more test stuff
          DIFF4(1) = ABS( ATMOS -  ATMO2)
          DIFF4(2) = ABS(DATMOS - DATMO2)
          DIFF4(3) = ABS( CLOCK(1) -  CLOC2(1))
          DIFF4(4) = ABS(DCLOCK(1) - DCLOC2(1))
C           DO J=1,4
C            IF (DIFF4(J) .GT. 0.0) IPRNT = IPRNT+1
C           ENDDO
           IF (DIFF4(1) .GE. 1.0E-11) IPRNT = IPRNT+1
           IF (DIFF4(2) .GE. 1.0E-12) IPRNT = IPRNT+1
           IF (DIFF4(3) .GE. 1.0E-8 ) IPRNT = IPRNT+1
           IF (DIFF4(4) .GE. 1.0E-9 ) IPRNT = IPRNT+1
C
C          IF(IPRNT .GT. 1) THEN
C           WRITE(10,1087) JROW,CLANT,SOURCL,CTIME,TMPTIM,DIFF6,DIFF4
C           WRITE(6,1087)  JROW,CLANT,SOURCL,CTIME,TMPTIM,DIFF6,DIFF4
 1087       FORMAT(I6,I3,I3,F6.3,E10.2,1X,6E10.2,2X,4E10.2)
C          ENDIF
C
C  Write output record for this row
C
         KROW = JROW
C        KROW = KROW + 1
         CALL OTABCL (OUTTAB, 'WRIT', KROW, NPOLN, NUMIF, CTIME,
     *      TIMEI, SOURCL, CLANT, SUBA, FREQID, IFR, GEODL2, DOPOFF,
     *      ATMO2, DATMO2, MBDELY, CLOC2, DCLOC2, DISP, DDISP, CREAL,
     *      CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
           IF(IERR.NE.0) THEN
             PRINT *,'JROW,KROW,OTABCL error = ',JROW,KROW,IERR
             RETURN
           ENDIF
C
 310     CONTINUE
C
 1000 CONTINUE
C
C       CLOSE(10)
C
        RETURN
        END
C-----------------------------------------------------------------------------
      SUBROUTINE GETIM(IMREC, RSTRT, CLANT, SOURCL, GTIME, GDELA )
C-----------------------------------------------------------------------------
C   Reads the requested IM record.
C
C     Inputs:
C      IMREC    I   Requested record number in the IM table
C      RSTRT    D   IM time should match this time.
C      CLANT    I   Antenna should match this number (from CL table).
C      SOURCL   I   Source should match this number (from CL table).
C
C     Outputs:
C      GTIME    D   IM table time
C      GDELA(6) D   The six polynomial coeffiecients from the IM table.
C-----------------------------------------------------------------------------
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
      INTEGER IMREC, SOURIM, IMANT, I, CLANT, SOURCL
      DOUBLE PRECISION GTIME, GDELA(6), RSTRT
      DOUBLE PRECISION IMTIM(3000)
      REAL IMTINT
      INTEGER IMSRC(3000), IMSUB(3000), IMROW(3000,20), IMKNT
     *        ,MCROW(3000,20)
      COMMON /IMMAP/ IMTIM, IMSRC, IMSUB, IMROW, MCROW, IMTINT, IMKNT
      INTEGER BUFFIM(1024)
      INTEGER NTERM, INVER, IRET
      INTEGER DISKIN, SEQIN, OLDCNO, NUSER, LUN
      CHARACTER NAMEIN*12, CLAIN*6
      COMMON /IMSTUF/ DISKIN, SEQIN, OLDCNO, NUSER, LUN, NAMEIN, CLAIN
C
       INVER = 1
       NTERM = 6
C
       LUN = 38
        CALL IMINI ('READ', BUFFIM, DISKIN, OLDCNO, INVER, CATBLK, LUN,
     *       IRET)
C
           IIMRNO = IMREC
         CALL IMTAB ('READ', BUFFIM, NOBAND, IRET )
          IF(IRET.NE.0) THEN
           PRINT *, ' GETIM/IMTAB: IRET, IMREC ', IRET, IMREC
           GO TO 997
          ENDIF
C
           GTIME = TIME
           SOURIM = ISRC
           IMANT = NOSTA
C  Double check the match
         IF(DABS(RSTRT-GTIME) .GT. 1.D-9)
     *     PRINT *,'GETIM time mismatch: RSTRT, GTIME = ', RSTRT, GTIME
         IF(IMANT .NE. CLANT)
     *     PRINT *,'GETIM Antenna mismatch: IMANT, CLANT ', IMANT, CLANT
         IF(SOURIM .NE. SOURCL)
     *    PRINT *,'GETIM Source mismatch: SOURIM, SOURCL ',SOURIM,SOURCL
C  Geometric delay terms (the correlator model).
         DO 52 I = 1, 6
          GDELA(I) = GDELA1(I)
 52      CONTINUE
C
        CALL TABIO('CLOS',1,IIMRNO,BUFFIM,BUFFIM,IRET)
C
 997    CONTINUE
        RETURN
        END
C
C*****************************************************************************
      SUBROUTINE GTMC (MCREC, CLANT, SOURCL, MTIME, ATMO, DATMO,
     *           KLOK, DKLOK)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
C
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PAOOF.INC'
C     INCLUDE 'INCS:DMSG.INC'
C     INCLUDE 'INCS:DCAT.INC'
      INTEGER   TBKOLS(128,MAXIO), TBNUMV(128,MAXIO)
      COMMON /TBSTUF/ TBKOLS, TBNUMV
C     INTEGER MCCOLS(21)
C     EQUIVALENCE ( TBKOLS(1,1),MCCOLS(1) )
C
      INTEGER IRNO, RESULT(512), MCREC, JFRST, CLANT, SOURCL, I1
      REAL RES4(512)
      DOUBLE PRECISION RES8(256), MTIME, ATMO, DATMO, KLOK, DKLOK
      EQUIVALENCE (RESULT, RES4, RES8)
      INTEGER SOURMC, MCANT
C
      DOUBLE PRECISION IMTIM(3000)
      REAL IMTINT
      INTEGER IMSRC(3000), IMSUB(3000), IMROW(3000,20), IMKNT
     *        ,MCROW(3000,20)
      COMMON /IMMAP/ IMTIM, IMSRC, IMSUB, IMROW, MCROW, IMTINT, IMKNT
      INTEGER BUFFIM(512), NTERM, INVER, IRET
      INTEGER DISKIN, SEQIN, OLDCNO, NUSER, LUN
      CHARACTER NAMEIN*12, CLAIN*6
      COMMON /IMSTUF/ DISKIN, SEQIN, OLDCNO, NUSER, LUN, NAMEIN, CLAIN
      COMMON /I1MC/I1
      DATA JFRST /0/
C-----------------------------------------------------------------------
       INVER = 1
       NTERM = 6
       LUN = 37
C
C  Initialize MC table
       CALL TABINI ('READ', 'MC', DISKIN, OLDCNO, INVER, CATBLK, LUN,
     *      NKEY, NREC, NCOL, DATP, BUFFIM, IRET)
       IF(IRET .NE. 0) RETURN
C
C       I1 = BUFFIM(8) - 4
C
        IRNO = MCREC
        CALL TABIO ('READ', 0, IRNO, RESULT, BUFFIM, IRET)
       IF(IRET .NE. 0) RETURN
C
        SOURMC = RESULT(I1)
        MCANT  = RESULT(I1+1)
C
C       SOURMC = RESULT(TBKOLS(2,1))
C       MCANT  = RESULT(TBKOLS(3,1))
C
         IF (MCANT .NE. CLANT)
     *     PRINT *,'GTMC Antenna mismatch: MCANT, CLANT ', MCANT, CLANT
        MTIME  = RES8(1)
        ATMO   = RES8(2)
        DATMO  = RES8(3)
        KLOK   = RES8(6)
        DKLOK  = RES8(7)
C
C       MTIME  = RES8(TBKOLS(1,1))
C       ATMO   = RES8(TBKOLS(6,1))
C       DATMO  = RES8(TBKOLS(7,1))
C       KLOK   = RES8(TBKOLS(10,1))
C       DKLOK  = RES8(TBKOLS(11,1))
C
        CALL TABIO('CLOS',1,IRNO,BUFFIM,BUFFIM,IRET)
C
        RETURN
        END
