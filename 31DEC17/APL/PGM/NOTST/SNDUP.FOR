      PROGRAM SNDUP
C-----------------------------------------------------------------------
C! Task to copy single-pol SN table to dual-pol, duplicating entries
C# EXT-util Utility Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2015
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
C  SNDUP copies & duplicates SN tables.  The specified
C  output table should not exist before the execution of SNDUP.
C   ADVERBS:
C    INNAME      Input image name (name)
C    INCLASS     Input image name (class)
C    INSEQ       Input image name (seq. #)
C    INDISK      Input image disk unit #
C    INVERS      Input table file version no.
C    OUTNAME     Output image name (name)
C    OUTCLASS    Output image name (class)
C    OUTSEQ      Output image name (seq. #)
C    OUTDISK     Output image disk unit #.
C    OUTVERS     Output table file version.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      CHARACTER  NAMEIN*12, CLAIN*6, FTYPE*2, STAT*4, ATIME*8, ADATE*12,
     *   NAMOUT*12, CLAOUT*6, PRGM*6, TABTYP*2
      HOLLERITH XNAMIN(3), XCLAIN(2), XNMOUT(3), XCLOUT(2),
     *   CATHLD(256)
      INTEGER  IRET, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), BUFF1(1024), BUFF2(1024), IERR, IVER, OVER, LUN1,
     *   LUN2, NPARM, IROUND, LOOP, IRCODE, J, NREC, NOLDT, TIME(3),
     *   DATE(3), NPOLI, NPOLO, NIFI, NIFO
      INTEGER  ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NUMPOL,
     *   OUTPOL, NUMIF, NUMNOD, OSNRNO, SNOKLS(MAXSNC), SNONMV(MAXSNC),
     *   SNRECI(10+15*MAXIF), OPRECI(10+15*MAXIF),
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   MB1SN, DS1SN, DD1SN, RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   MB2SN, DS2SN, DD2SN, RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN,
     *   TIMOP, INTOP, SOUOP, ANTOP, SUBOP, FRQOP, IFROP, NODOP,
     *   MB1OP, DS1OP, DD1OP, RE1OP, IM1OP, DL1OP, RA1OP, WT1OP, RF1OP,
     *   MB2OP, DS2OP, DD2OP, RE2OP, IM2OP, DL2OP, RA2OP, WT2OP, RF2OP
      REAL     GMMOD, RANOD(25), DECNOD(25), SNRECR(10+15*MAXIF),
     *   OPRECR(10+15*MAXIF)
      DOUBLE PRECISION SNRECD(10+15*MAXIF), OPRECD(10+15*MAXIF)
      LOGICAL   ISAPL
      REAL      XSEQIN, XDISKI, XIVER, XSEQO, XDISKO, XOVER
      LOGICAL   T, SAME
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XIVER,
     *   XNMOUT, XCLOUT, XSEQO, XDISKO, XOVER
      EQUIVALENCE (CATOLD, CATHLD)
      EQUIVALENCE (SNRECI, SNRECR, SNRECD)
      EQUIVALENCE (OPRECI, OPRECR, OPRECD)
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA PRGM /'SNDUP '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 16
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMIN, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLAOUT)
C                                       Crunch input parameters.
      SEQIN  = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO  = IROUND (XDISKO)
      IVER   = IROUND (XIVER)
      OVER   = IROUND (XOVER)
      XOVER  = OVER
      TABTYP = 'SN'
C                                       Prevent write to disk # 0 !
      IF (DISKO .EQ. 0) DISKO = DISKIN
C                                       Default output = input.
      IF (NAMOUT .EQ. ' ') NAMOUT = NAMEIN
      IF (CLAOUT .EQ. ' ') CLAOUT = CLAIN
C                                       Find input
 30   OLDCNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, FTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Find output
      NEWCNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKO, NEWCNO, NAMOUT, CLAOUT, SEQOUT, FTYPE,
     *    NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMOUT, CLAOUT, SEQOUT, DISKO,
     *      NLUSER
         GO TO 990
         END IF
      SAME = (DISKIN.EQ.DISKO) .AND. (OLDCNO.EQ.NEWCNO)
      STAT = 'READ'
      IF (SAME) STAT = 'REST'
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, STAT, BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      IF (.NOT.SAME) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         END IF
C
      CALL COPY (256, CATOLD, CATBLK)
      CALL UVPGET (IERR)
      NPOLI = CATBLK(KINAX+JLOCS)
      NIFI = CATBLK(KINAX+JLOCIF)
C                                       Read new CATBLK and mark 'WRIT'
      CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 1
C                                       Check number of tables
      CALL FNDEXT (TABTYP, CATOLD, NOLDT)
C                                       use highest version when IVER = 0
      IF (IVER.LE.0) IVER = NOLDT
      XIVER = IVER
C                                       Check we are legal
      CALL UVPGET (IERR)
      NPOLO = CATBLK(KINAX+JLOCS)
      NIFO = CATBLK(KINAX+JLOCIF)
      IF (NPOLI.NE.1) THEN
         WRITE (MSGTXT,1010) NPOLI
         IERR = 1
         GO TO 990
         END IF
      IF (NPOLO.EQ.1) THEN
         WRITE (MSGTXT,1020) NPOLO
         IERR = 1
         GO TO 990
         END IF
      IF (NIFI.NE.NIFO) THEN
         WRITE (MSGTXT,1025) NIFI, NIFO
         IERR = 1
         GO TO 990
         END IF
C
      IRCODE = 0
C                                       Open input table
      CALL SNINI ('READ', BUFF1, DISKIN, OLDCNO, IVER, CATOLD,
     *   LUN1, ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,
     *   NUMNOD, GMMOD, RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       # records
      NREC = BUFF1(5)
C                                       Set table pointers
      TIMSN = SNKOLS(SNDTIM)
      INTSN = SNKOLS(SNRTMI)
      SOUSN = SNKOLS(SNISID)
      ANTSN = SNKOLS(SNIANT)
      SUBSN = SNKOLS(SNISUB)
      FRQSN = SNKOLS(SNIFQI)
      IFRSN = SNKOLS(SNRIFR)
      NODSN = SNKOLS(SNINOD)
      MB1SN = SNKOLS(SNRMD1)
      DS1SN = SNKOLS(SNRDS1)
      DD1SN = SNKOLS(SNRDD1)
      RE1SN = SNKOLS(SNRRE1)
      IM1SN = SNKOLS(SNRIM1)
      RA1SN = SNKOLS(SNRRA1)
      DL1SN = SNKOLS(SNRDE1)
      WT1SN = SNKOLS(SNRWE1)
      RF1SN = SNKOLS(SNIRF1)
      MB2SN = SNKOLS(SNRMD2)
      DS2SN = SNKOLS(SNRDS2)
      DD2SN = SNKOLS(SNRDD2)
      RE2SN = SNKOLS(SNRRE2)
      IM2SN = SNKOLS(SNRIM2)
      RA2SN = SNKOLS(SNRRA2)
      DL2SN = SNKOLS(SNRDE2)
      WT2SN = SNKOLS(SNRWE2)
      RF2SN = SNKOLS(SNIRF2)
C                                       Open output table
      OUTPOL = 2
      CALL SNINI ('WRIT', BUFF2, DISKO, NEWCNO, OVER, CATBLK,
     *   LUN2, OSNRNO, SNOKLS, SNONMV, NUMANT, OUTPOL, NUMIF,
     *   NUMNOD, GMMOD, RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Relevant o/p table pointers
      TIMOP = SNOKLS(SNDTIM)
      INTOP = SNOKLS(SNRTMI)
      SOUOP = SNOKLS(SNISID)
      ANTOP = SNOKLS(SNIANT)
      SUBOP = SNOKLS(SNISUB)
      FRQOP = SNOKLS(SNIFQI)
      IFROP = SNOKLS(SNRIFR)
      NODOP = SNOKLS(SNINOD)
      MB1OP = SNKOLS(SNRMD1)
      DS1OP = SNKOLS(SNRDS1)
      DD1OP = SNKOLS(SNRDD1)
      RE1OP = SNOKLS(SNRRE1)
      IM1OP = SNOKLS(SNRIM1)
      RA1OP = SNOKLS(SNRRA1)
      DL1OP = SNOKLS(SNRDE1)
      WT1OP = SNOKLS(SNRWE1)
      RF1OP = SNOKLS(SNIRF1)
      MB2OP = SNKOLS(SNRMD2)
      DS2OP = SNKOLS(SNRDS2)
      DD2OP = SNKOLS(SNRDD2)
      RE2OP = SNOKLS(SNRRE2)
      IM2OP = SNOKLS(SNRIM2)
      RA2OP = SNOKLS(SNRRA2)
      DL2OP = SNOKLS(SNRDE2)
      WT2OP = SNOKLS(SNRWE2)
      RF2OP = SNOKLS(SNIRF2)
C                                       Loop, copy and duplicate
      DO 80 LOOP = 1, NREC
         ISNRNO = LOOP
         CALL TABIO ('READ', IRCODE, ISNRNO, SNRECR, BUFF1, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
            END IF
         IF (IERR.LT.0) GO TO 90
C
         OPRECD(TIMOP) = SNRECD(TIMSN)
         OPRECR(INTOP) = SNRECR(INTSN)
         OPRECI(SOUOP) = SNRECI(SOUSN)
         OPRECI(ANTOP) = SNRECI(ANTSN)
         OPRECI(SUBOP) = SNRECI(SUBSN)
         OPRECI(FRQOP) = SNRECI(FRQSN)
         OPRECR(IFROP) = SNRECR(IFRSN)
         OPRECI(NODOP) = SNRECI(NODSN)
         IF (MB1SN.GT.0) OPRECI(MB1OP) = SNRECI(MB1SN)
         IF (MB1SN.GT.0) OPRECI(MB2OP) = SNRECI(MB1SN)
         IF (DS1SN.GT.0) OPRECI(DS1OP) = SNRECI(DS1SN)
         IF (DS1SN.GT.0) OPRECI(DS2OP) = SNRECI(DS1SN)
         IF (DD1SN.GT.0) OPRECI(DD1OP) = SNRECI(DD1SN)
         IF (DD1SN.GT.0) OPRECI(DD2OP) = SNRECI(DD1SN)
         DO 50 J = 1, NUMIF
            OPRECR(RE1OP+J-1) = SNRECR(RE1SN+J-1)
            OPRECR(RE2OP+J-1) = SNRECR(RE1SN+J-1)
            OPRECR(IM1OP+J-1) = SNRECR(IM1SN+J-1)
            OPRECR(IM2OP+J-1) = SNRECR(IM1SN+J-1)
            OPRECR(RA1OP+J-1) = SNRECR(RA1SN+J-1)
            OPRECR(RA2OP+J-1) = SNRECR(RA1SN+J-1)
            OPRECR(DL1OP+J-1) = SNRECR(DL1SN+J-1)
            OPRECR(DL2OP+J-1) = SNRECR(DL1SN+J-1)
            OPRECR(WT1OP+J-1) = SNRECR(WT1SN+J-1)
            OPRECR(WT2OP+J-1) = SNRECR(WT1SN+J-1)
            OPRECI(RF1OP+J-1) = SNRECI(RF1SN+J-1)
            OPRECI(RF2OP+J-1) = SNRECI(RF1SN+J-1)
 50         CONTINUE
C
         OSNRNO = ISNRNO
         CALL TABIO ('WRIT', IRCODE, OSNRNO, OPRECR, BUFF2, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1080) IERR
            GO TO 990
            END IF
C
 80      CONTINUE
C
 90   IRET = 0
C                                       Close tables.
      CALL TABIO ('CLOS', IRCODE, LOOP, SNRECR, BUFF1, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1090) IERR
         GO TO 990
         END IF
      CALL TABIO ('CLOS', IRCODE, LOOP, OPRECR, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
C                                       Add history to output
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKO, NEWCNO, BUFF1, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (6)
         GO TO 150
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (MSGTXT,1120) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Input file
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN1, BUFF1,
     *   IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Type and version
      IVER = IROUND (XIVER)
      OVER = IROUND (XOVER)
      WRITE (MSGTXT,2000) TSKNAM, TABTYP, IVER, OVER
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Close HI file
 150  CALL HICLOS (LUN1, T, BUFF1, IERR)
      GO TO 995
C                                       Error
 990  CALL MSGWRT (8)
C                                       Close down files, etc
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETING INPUT PARAMETERS')
 1010 FORMAT (I3,' POLZNS IN INPUT FILE - SNDUP EXPECTS 1')
 1020 FORMAT (I3,' POLZNS IN OUTPUT FILE - SNDUP EXPECTS 2 OR MORE')
 1025 FORMAT ('# IFS IN I/P AND O/P FILES DIFFER: ',I3,2X,I3)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' OPENING SN TABLE FOR READ')
 1060 FORMAT ('ERROR',I3,' OPENING SN TABLE FOR WRITE')
 1070 FORMAT ('ERROR',I3,' READING SN TABLE')
 1080 FORMAT ('ERROR',I3,' WRITING SN TABLE')
 1090 FORMAT ('ERROR',I3,' CLOSING INPUT SN TABLE')
 1100 FORMAT ('ERROR',I3,' CLOSING OUTPUT SN TABLE')
 1110 FORMAT ('ERROR',I3,' OPENING HISTORY FILE')
 1120 FORMAT (A6,'Release =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,' INEXT=''',A2,''' INVERS=',I4,' OUTVERS=', I4)
      END
