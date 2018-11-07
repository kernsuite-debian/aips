LOCAL INCLUDE 'CSCOR.INC'
C                                       Local include for CSCOR
C                                       Needs parameter from PUVD.INC
C                                       Inputs and general info
      INTEGER   SEQIN, SUBA, DISKIN, CNOIN, NUMHIS, CSVER, CSVOUT,
     *   NANTSL, ANTENS(50), BIF, EIF, ISTOK, FRQSEL
      LOGICAL   DOAWNT
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSTOK, XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, XSTOK*4, OPCODE*4, HISCRD(20)*64
      REAL      XSIN, XDISIN, XBIF, XEIF, XTIME(8), XANT(50), XSUBA,
     *   XGVER, XGUSE, BPARM(10), CPARM(10), XBAD(10), TIMBEG, TIMEND
      DOUBLE PRECISION FRQOFF(MAXIF)
C                                       Buffers and file info
      INTEGER   BUFFER(1024), BUFOUT(1024)
C                                       Important constants
      DOUBLE PRECISION PI, TWOPI, SIDER, CLIGHT
      COMMON /CHRCOM/  HISCRD, NAMEIN, CLAIN, XSTOK, OPCODE
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSTOK, XBIF, XEIF,
     *   XTIME, XANT,  XSUBA, XGVER, XGUSE, XOPCOD, BPARM, CPARM, XBAD,
     *   SEQIN, DISKIN, CNOIN, SUBA, CSVER, CSVOUT, TIMBEG, TIMEND
      COMMON /CINFO/ FRQOFF, DOAWNT, NANTSL, ANTENS, BIF, EIF, ISTOK,
     *   NUMHIS, FRQSEL
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER, BUFOUT
C                                       Important constants
      COMMON /CONST/ PI, TWOPI, SIDER, CLIGHT
C                                                          End CSCOR
LOCAL END
      PROGRAM CSCOR
C-----------------------------------------------------------------------
C! Applies calibration corrections to a CS table
C# Calibration EXT-util Sdish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2012
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
C   Task CSCOR applies corrections to CS tables.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CSCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'CSCOR '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL CSCLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Apply corrections
      CALL CSCUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL CSCLHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
 999  STOP
      END
      SUBROUTINE CSCLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   CSCLIN gets input parameters for CSCOR.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, CHR*4, CHL*4, PTYPE*2, RDATE*8
      LOGICAL   T, F, EQUAL, ALLANT, DESEL
      INTEGER   JERR, NPARM, IERR, I, NEXT, IARG, LIMIT, J,
     *   IROUND, LUN, NUMIF, IIVER, DUMMY(1)
      DOUBLE PRECISION JDA, GASTM, DTOR, GSTIA0, DEGPDY
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CSCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      EQUIVALENCE (DUMMY, STNELP)
      DATA CHR, CHL /'R ','L '/
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /29/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFFER)
      CALL VHDRIN
      NUMHIS = 0
C                                       Set important constants
      PI = 3.1415926536D0
      TWOPI = 6.2831853072D0
      SIDER = 1.002737923D0
      CLIGHT = 2.997925D8
      DTOR = TWOPI / 360.0D0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: Was 102
      NPARM = 92
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      IF (SUBA.LE.0) SUBA = 1
      CSVER = IROUND (XGVER)
      CSVOUT = IROUND (XGUSE)
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       Find file, read CATBLK
 30   CNOIN = 1
      STAT = 'SRCH'
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, PTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
 40   CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 45
         WRITE (MSGTXT,1040) IERR
         GO TO 990
 45   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
 50   CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      NRPARM = CATBLK(KIPCN)
      FRQSEL = -1
C                                       IF range
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
         END IF
C                                       Stokes' type.
      ISTOK = 0
      EQUAL = XSTOK .EQ. CHR
      IF (EQUAL) ISTOK = 1
      EQUAL = XSTOK .EQ. CHL
      IF (EQUAL) ISTOK = 2
C                                       Check sort order of input
      EQUAL = ISORT(:1).EQ.'T'
      IF (EQUAL) GO TO 70
         WRITE (MSGTXT,1060) ISORT
         JERR = 1
         GO TO 990
 70   JERR = 0
C                                       Antenna list
      ALLANT = T
      DESEL = F
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 100     CONTINUE
      NEXT = 1
      IF (ALLANT) GO TO 160
C                                       Not all selected - make list
C                                       ANTENNAS array.
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.EQ.0) GO TO 150
C                                       See if already have
               LIMIT = NEXT - 1
               IF (LIMIT.LT.1) GO TO 140
               DO 130 J = 1,LIMIT
                  IF (IARG.EQ.ANTENS(J)) GO TO 150
 130              CONTINUE
C                                       New antenna
 140              ANTENS(NEXT) = IARG
                  NEXT = NEXT + 1
 150           CONTINUE
 160  DOAWNT = .NOT. DESEL
      NANTSL = NEXT - 1
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBA, CATBLK, BUFFER, JERR)
C                                       Get GST0 and Earth rotation rate
      IF (JERR.NE.0) THEN
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JDA)
         CALL GSTROT (JDA, GSTIA0, GASTM, DEGPDY)
         GSTIAT = GSTIA0 * DTOR
         ROTIAT = DEGPDY * DTOR
         DO 165 J = 1,MAXANT
            STNLON(J) = CPARM(1) * DTOR
            STNLAT(J) = CPARM(2) * DTOR
 165        CONTINUE
         IF ((CPARM(1).EQ.0.0) .OR. (CPARM(2).EQ.0.0)) GO TO 999
         JERR = 0
         END IF
C                                       Get IF information
      NUMIF = 1
      FRQOFF(1) = 0.0
      IIVER = 1
      IF (JLOCIF.GE.0)
     *   CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FRQSEL, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       CSVOUT=1 disallowed
      IF (CSVOUT.GT.1) GO TO 170
         JERR = 6
         WRITE (MSGTXT,1160)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1161)
         GO TO 990
 170     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CSCLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('INPUT VIS RECORDS MISORDERED, SORTED = ',A2,
     *        ' SHOULD BE = TB')
 1160 FORMAT ('MODIFYING CS TABLE VER. 1 IS NOT ALLOWED; USE TACOP')
 1161 FORMAT ('IF NECESSARY TO MAKE A HIGHER NUMBERED CS TABLE')
      END
      SUBROUTINE CSCUV (IERR)
C-----------------------------------------------------------------------
C   CSCUV is called from CSCOR. CSCUV reads throught the CS table,
C   passing the records selected to the correction routine CSCCOR.
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   CSRECI(5+8*MAXIF), LUN, LUN2, CSKOLS(13), CSNUMV(13),
     *   NUMPOL, NUMIF, ICODE, IRCODE, ANT, I, JERR, TIMKOL, RAKOL,
     *   DECKOL, BEMKOL, SUBKOL, F1KOL, O1KOL, RF1KOL, DF1KOL, F2KOL,
     *   O2KOL, RF2KOL, DF2KOL, ICSRNO, OCSRNO, NUMREC, LOOP, FIXCNT,
     *   NUMBEM
      LOGICAL   MAT, F, SAME
      REAL      CSRECR(5+8*MAXIF), TEMP(20), PANGLE(MAXANT), COSDEC,
     *   SINDEC
      DOUBLE PRECISION CSRECD(3+4*MAXIF)
      INCLUDE 'CSCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /CSRECC/  COSDEC, SINDEC, CSRECR, TEMP, PANGLE, FIXCNT,
     *   CSKOLS, CSNUMV, NUMBEM, NUMPOL, NUMIF, ICODE
      EQUIVALENCE (CSRECI, CSRECR, CSRECD)
      EQUIVALENCE (CSKOLS(1), TIMKOL),
     *   (CSKOLS(2),RAKOL), (CSKOLS(3), DECKOL),
     *   (CSKOLS(4),BEMKOL), (CSKOLS(5), SUBKOL),
     *   (CSKOLS(6), F1KOL), (CSKOLS(7),O1KOL),
     *   (CSKOLS(8), RF1KOL), (CSKOLS(9), DF1KOL),
     *   (CSKOLS(10), F2KOL), (CSKOLS(11),O2KOL),
     *   (CSKOLS(12), RF2KOL), (CSKOLS(13), DF2KOL)
      DATA LUN, LUN2 /28,29/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      FIXCNT = 0
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
C                                       Open CS table
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Input
      CALL CSINI ('READ', BUFFER, DISKIN, CNOIN, CSVER, CATBLK, LUN,
     *   ICSRNO, CSKOLS, CSNUMV, NUMBEM, NUMPOL, NUMIF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       See if input = output file.
      SAME = CSVER.EQ.CSVOUT
C                                       If SAME close.
      IF (SAME) THEN
         CALL TABIO ('CLOS', IRCODE, LOOP, CSRECR, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Output
      CALL CSINI ('WRIT', BUFOUT, DISKIN, CNOIN, CSVOUT, CATBLK, LUN2,
     *   OCSRNO, CSKOLS, CSNUMV, NUMBEM, NUMPOL, NUMIF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       See if input = output file.
      SAME = CSVER.EQ.CSVOUT
C                                       Get number of records
      IF (SAME) THEN
         NUMREC = BUFOUT(5)
      ELSE
         NUMREC = BUFFER(5)
         END IF
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
C                                       Initial call to CSCCOR
      CALL CSCCOR (1, IERR)
      IF (IERR.NE.0) GO TO 999
      OCSRNO = OCSRNO - 1
C                                       Update table
      DO 500 LOOP = 1,NUMREC
         IF (SAME) THEN
            OCSRNO = LOOP
            CALL TABIO ('READ', IRCODE, OCSRNO, CSRECR, BUFOUT, IERR)
            OCSRNO = LOOP
         ELSE
            ICSRNO = LOOP
            CALL TABIO ('READ', IRCODE, ICSRNO, CSRECR, BUFFER, IERR)
            OCSRNO = OCSRNO + 1
            END IF
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 500
C                                       Check data
C                                       Time:
         IF ((CSRECR(TIMKOL).LT.TIMBEG) .OR.
     *       (CSRECR(TIMKOL).GT.TIMEND)) GO TO 500
C                                       Subarray
         IF (CSRECI(SUBKOL).NE.SUBA) GO TO 500
C                                       See if all beams desired
         IF (NANTSL.LE.0) GO TO 130
            ANT = CSRECI(BEMKOL)
            MAT = F
            DO 120 I = 1,NANTSL
               MAT = MAT .OR. (ANT.EQ.ANTENS(I))
 120           CONTINUE
C                                       Check for match selected.
         IF (DOAWNT .AND. MAT) GO TO 130
C                                       Check for match excluded
         IF (.NOT.DOAWNT .AND. MAT) GO TO 500
C                                       If inclusion ignore
         IF (DOAWNT) GO TO 500
C                                       Correct record.
 130     CALL CSCCOR (2, JERR)
         IF (JERR.NE.0) GO TO 500
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, OCSRNO, CSRECR, BUFOUT, IERR)
         IF (IERR.GT.0) GO TO 900
 500     CONTINUE
C                                       Final call to CSCCOR
         CALL CSCCOR (3, JERR)
C                                       Close tables.
 600  IF (.NOT.SAME) THEN
         CALL TABIO ('CLOS', IRCODE, LOOP, CSRECR, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL TABIO ('CLOS', IRCODE, LOOP, CSRECR, BUFOUT, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TOO MANY IFS SPECIFIED, ',I3,' > 10')
 1900 FORMAT ('TABIO ERROR',I3,' CORRECTING CS TABLE')
      END
      SUBROUTINE CSCCOR (IOP, IERR)
C-----------------------------------------------------------------------
C   CSCCOR applies corrections to the CS record passed thru common
C   /CSRECC/.
C   Input:
C    IOP        I    Operation code, 1=init, 2=process, 3=finish
C   Input from common:
C    CSREC(*)   I    The CS table record to be corrected.
C    BIF        I    First IF number
C    EIF        I    Highest IF number
C    ISTOK      I    Stokes number, 0=both, 1=first, 2=second.
C    OPCODE     C*4  Operation code.
C    BPARM(10)  R    parameters.
C   Output in common:
C    CSREC(*)   I    Modified record.
C   Output:
C    IERR       I    Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER OPS(4)*4
      INTEGER   IERR, IOP
      INTEGER   CSRECI(1),  CSKOLS(13), CSNUMV(13), NUMBEM, NUMPOL,
     *   NUMIF, ICODE, NOP, I, IBEAM, NTERMS, DATKOL, TIMKOL, RAKOL,
     *   DECKOL, BEMKOL, SUBKOL, F1KOL, O1KOL, RF1KOL, DF1KOL,
     *   F2KOL, O2KOL, RF2KOL, DF2KOL
      INTEGER   FIXCNT
      LOGICAL   EQUAL
      INCLUDE 'INCS:PUVD.INC'
      REAL      CSRECR(5+8*MAXIF), TEMP(20),
     *   PANGLE(MAXANT), XT, FACTOR, ZA, ARG, POLYN, ELV, HRANG,
     *   DARG, SINDEC, COSDEC, SINLAT, COSLAT, RAAPP,
     *   DECAPP, AZ, ZAZ
      DOUBLE PRECISION CSRECD(1)
      INCLUDE 'CSCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /CSRECC/ COSDEC, SINDEC,
     *   CSRECR, TEMP, PANGLE, FIXCNT,
     *   CSKOLS, CSNUMV, NUMBEM, NUMPOL, NUMIF, ICODE
      EQUIVALENCE (CSRECI, CSRECR, CSRECD)
      EQUIVALENCE (CSKOLS(1), TIMKOL),
     *   (CSKOLS(2),RAKOL), (CSKOLS(3), DECKOL),
     *   (CSKOLS(4),BEMKOL), (CSKOLS(5), SUBKOL),
     *   (CSKOLS(6), F1KOL), (CSKOLS(7),O1KOL),
     *   (CSKOLS(8), RF1KOL), (CSKOLS(9), DF1KOL),
     *   (CSKOLS(10), F2KOL), (CSKOLS(11),O2KOL),
     *   (CSKOLS(12), RF2KOL), (CSKOLS(13), DF2KOL)
C                        1       2     3      4
      DATA NOP, OPS /4,'OPAC','GAIN','PTRA','PTDC'/
C-----------------------------------------------------------------------
C                                       Determine operation
      IF (IOP.EQ.2) GO TO 200
      IF (IOP.EQ.3) GO TO 900
C                                       Initialize - find OPCODE
      ICODE = -1
      DO 30 I = 1,NOP
         EQUAL = OPS(I) .EQ. OPCODE
         IF (EQUAL) ICODE = I
 30      CONTINUE
      IF (ICODE.GT.0) GO TO 40
         IERR = 1
         WRITE (MSGTXT,1030) OPCODE
         GO TO 990
C                                       History - OPCODE
 40      NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2040) OPCODE
C                                       Setup
      GO TO (50,60,70,70,195), ICODE
C                                       Atmosphere opacity(1)
 50      TEMP(1) = BPARM(1)
C                                       Partial pressure of water (3)
         TEMP(2) = BPARM(2)
C                                       History
         NUMHIS = NUMHIS + 1
         IF (ICODE.EQ.2) WRITE (HISCRD(NUMHIS),2055) BPARM(1)
         IF (ICODE.EQ.3) WRITE (HISCRD(NUMHIS),2056) BPARM(1),
     *      BPARM(2)
         GO TO 999
C                                       Poly. gain curve (2)
 60      NTERMS = 0
         DO 65 I = 1,10
            TEMP(I+1) = BPARM(I)
            IF (ABS (BPARM(I)) .GT. 1.0E-20) NTERMS = I
 65         CONTINUE
         TEMP(1) = NTERMS
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2065) BPARM(1), BPARM(2), BPARM(3)
         IF (NTERMS.GT.3) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2066) (BPARM(I),I=4,7)
            END IF
         IF (NTERMS.GT.7) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2066) (BPARM(I),I=8,10)
            END IF
         GO TO 999
C                                       Poly pointing (ZA) (3,4)
 70      NTERMS = 0
         DO 75 I = 1,10
            TEMP(I+1) = BPARM(I)
            IF (ABS (BPARM(I)) .GT. 1.0E-20) NTERMS = I
 75         CONTINUE
         TEMP(1) = NTERMS
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2075) BPARM(1), BPARM(2), BPARM(3)
         IF (NTERMS.GT.3) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2076) (BPARM(I),I=4,7)
            END IF
         IF (NTERMS.GT.7) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2066) (BPARM(I),I=8,10)
            END IF
         GO TO 999
C                                       Undefined
 195     CONTINUE
         GO TO 999
C                                       Process record
 200  FIXCNT = FIXCNT + 1
      IBEAM = 1
      GO TO (250,350,400,400,850), ICODE
C                                       Atmosphere opacity(1)
 250     RAAPP = CSRECR(RAKOL) * 1.745329E-2
         DECAPP = CSRECR(DECKOL) * 1.745329E-2
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         HRANG = GSTIAT + STNLON(IBEAM) * 1.002738D0 + CSRECR(TIMKOL) *
     *      ROTIAT -  RAAPP
         COSLAT = COS (STNLAT(IBEAM))
         SINLAT = SIN (STNLAT(IBEAM))
         DARG = SINLAT * SINDEC + COSLAT * COSDEC * COS (HRANG)
         ZA = ACOS (DARG)
C                                       Transmission factor:
C                                       Modified cosecant law from
C                                       Chopo Ma's thesis:
         ELV = (1.570796327 - ZA)
         ARG = TEMP(1) / (DARG + (0.00143 / (TAN(ELV) + 0.0045)))
C                                       Three term approx. of exp.
         FACTOR = 1.0 + ARG * (1.0 + 0.5 * ARG )
         IF (ISTOK.EQ.2) GO TO 315
         DO 310 I = BIF,EIF
            XT = CSRECR(F1KOL+I-1)
            IF (XT.NE.FBLANK) CSRECR(F1KOL+I-1) = XT * FACTOR
 310        CONTINUE
 315        IF (ISTOK.EQ.1) GO TO 999
         DO 320 I = BIF,EIF
            XT = CSRECR(F2KOL+I-1)
            IF (XT.NE.FBLANK) CSRECR(F2KOL+I-1) = XT * FACTOR
 320        CONTINUE
         GO TO 999
C                                       Polynomial gain curve(2)
 350     RAAPP = CSRECR(RAKOL) * 1.745329E-2
         DECAPP = CSRECR(DECKOL) * 1.745329E-2
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
C                                       Compute zenith angle (deg).
         HRANG = GSTIAT + STNLON(IBEAM) * 1.002738D0 + CSRECR(TIMKOL) *
     *      ROTIAT -  RAAPP
         COSLAT = COS (STNLAT(IBEAM))
         SINLAT = SIN (STNLAT(IBEAM))
         DARG = SINLAT * SINDEC + COSLAT * COSDEC * COS (HRANG)
         ZA = ACOS (DARG) * 57.29577951D0
         NTERMS = TEMP(1) + 0.5
C                                       Polynomial expansion.
         FACTOR = POLYN (NTERMS, ZA, TEMP(2))
         IF (ISTOK.EQ.2) GO TO 370
         DO 360 I = BIF,EIF
            XT = CSRECR(F1KOL+I-1)
            IF (XT.NE.FBLANK) CSRECR(F1KOL+I-1) = XT / FACTOR
 360        CONTINUE
 370        IF (ISTOK.EQ.1) GO TO 999
         DO 380 I = BIF,EIF
            XT = CSRECR(F2KOL+I-1)
            IF (XT.NE.FBLANK) CSRECR(F2KOL+I-1) = XT / FACTOR
 380        CONTINUE
         GO TO 999
C                                       Pointing (ZA) curve (3,4)
 400     RAAPP = CSRECR(RAKOL) * 1.745329E-2
         DECAPP = CSRECR(DECKOL) * 1.745329E-2
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
C                                       Compute zenith angle (deg).
         HRANG = GSTIAT + STNLON(IBEAM) * 1.002738D0 + CSRECR(TIMKOL) *
     *      ROTIAT -  RAAPP
         COSLAT = COS (STNLAT(IBEAM))
         SINLAT = SIN (STNLAT(IBEAM))
         DARG = SINLAT * SINDEC + COSLAT * COSDEC * COS (HRANG)
         ZA = ACOS (DARG) * 57.29577951D0
         AZ = ATAN2 (-COSDEC * SIN (HRANG), (SINDEC*COSLAT -
     *      COSDEC*COS (HRANG)*SINLAT))
         ZAZ = ZA * COS (AZ)
         NTERMS = TEMP(1) + 0.5
C                                       Polynomial expansion.
         FACTOR = POLYN (NTERMS, ZAZ, TEMP(2)) * 2.77777778E-4
         IF (ICODE.EQ.3) THEN
C                                       RA*cos(dec)
            DATKOL = RF1KOL
         ELSE
C                                       Declination
            DATKOL = DF1KOL
            END IF
         IF (ISTOK.NE.2) THEN
            DO 410 I = BIF,EIF
               XT = CSRECR(DATKOL+I-1)
               IF (XT.NE.FBLANK) CSRECR(DATKOL+I-1) = XT + FACTOR
 410           CONTINUE
            END IF
         IF (ISTOK.EQ.1) GO TO 999
         IF (ICODE.EQ.3) THEN
C                                       RA*cos(dec)
            DATKOL = RF2KOL
         ELSE
C                                       Declination
            DATKOL = DF2KOL
            END IF
         DO 420 I = BIF,EIF
            XT = CSRECR(DATKOL+I-1)
            IF (XT.NE.FBLANK) CSRECR(DATKOL+I-1) = XT + FACTOR
 420        CONTINUE
         GO TO 999
C                                       Undefined
 850     CONTINUE
         GO TO 999
C                                       Finish - number changed.
 900  NUMHIS= NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2900) FIXCNT
      WRITE (MSGTXT,2901) FIXCNT
      CALL MSGWRT (6)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('ERROR: UNKNOWN OPCODE: ',A4)
 1040 FORMAT ('ERROR: UNKNOWN CORRECTION MODE: ',F3.0)
 2040 FORMAT ('OPCODE = ''',A4,''' / OPERATION CODE')
 2055 FORMAT ('BPARM(1)=',F8.2,' / ZENITH OPACITY')
 2056 FORMAT ('BPARM=',F8.2,',',F8.2,' / ATM. PRES. (MBARS), PART.',
     *   ' PRES. H2O')
 2065 FORMAT ('BPARM =',1PE12.5,2(',',E12.5),' / GAIN CURVE')
 2066 FORMAT ('      ,',1PE12.5,3(',',E12.5))
 2075 FORMAT ('BPARM =',1PE12.5,2(',',E12.5),' / POINT (ZA)')
 2076 FORMAT ('      ,',1PE12.5,3(',',E12.5))
 2900 FORMAT (' / ',I6,' RECORDS MODIFIED')
 2901 FORMAT (I6,' RECORDS MODIFIED')
      END
      SUBROUTINE CSCLHI
C-----------------------------------------------------------------------
C   CSCLHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, LABEL*8, HILINE*72
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CSCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
C                                       Task message
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Beams
 25      WRITE (HILINE,3005) TSKNAM
         IF (NANTSL.LE.0) CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (NANTSL.LE.0) GO TO 35
C                                       Included or excluded?
         WRITE (HILINE,3006) TSKNAM
         IF (DOAWNT) WRITE (HILINE,3007) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       1st 12 and label.
         LIMIT = MIN (12, NANTSL)
         WRITE (HILINE,3008) TSKNAM, (ANTENS(J),J=1,LIMIT)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (NANTSL.LE.12) GO TO 35
C                                       Rest of beams
         DO 30 I = 13,NANTSL,12
            LIMIT = I
            LIMIT2 = I + 11
            LIMIT2 = MIN (NANTSL, LIMIT2)
            WRITE (HILINE,3009) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 30         CONTINUE
C                                       TIMERANG
 35   CALL HITIME (TIMBEG, TIMEND, LUN1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Stokes'
      WRITE (HILINE,2005) TSKNAM, XSTOK
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF range
      WRITE (HILINE,2004) TSKNAM, BIF, EIF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SUBARRAY, GAINVER
      WRITE (HILINE,2002) TSKNAM, SUBA, CSVER
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add history from common
         IF (NUMHIS.LE.0) GO TO 100
         WRITE (LABEL,1011) TSKNAM
         DO 90 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 90         CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN1, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CSCLHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* START ',A12,2X,A8)
 1011 FORMAT (A6,'  ')
 2002 FORMAT (A6, ' SUBARRAY =',I3,' GAINVER = ',I4,' /CS TABLE')
 2004 FORMAT (A6,' BIF =',I4,', EIF =',I4,'/ IF RANGE')
 2005 FORMAT (A6,' STOKES = ''',A4,''' / STOKES TYPE')
 3005 FORMAT (A6,' BEAMS = 0     /ALL BEAMS SELECTED')
 3006 FORMAT (A6,' /BEAMS EXCLUDED:')
 3007 FORMAT (A6,' /BEAMS INCLUDED:')
 3008 FORMAT (A6,' BEAMS = ',12(I3,' '))
 3009 FORMAT (A6,'            ',12(I3,' '))
      END
      REAL FUNCTION POLYN (NTERMS, ARG, COEF)
C-----------------------------------------------------------------------
C   Evaluates a polynomial function.
C    Inputs:
C     NTERMS    I    Number of terms (coefficients).
C     ARG       R    Argument of polynomial expansion.
C     COEF(*)   R    Coefficients.
C-----------------------------------------------------------------------
      INTEGER   NTERMS
      REAL      ARG, COEF(*)
C
      INTEGER   LOOP
      REAL      TEMP, SUM
C-----------------------------------------------------------------------
      SUM = COEF(1)
      TEMP = 1.0
      DO 100 LOOP = 2,NTERMS
         TEMP = TEMP * ARG
         SUM = SUM + COEF(LOOP) * TEMP
 100     CONTINUE
      POLYN = SUM
C
 999  RETURN
      END
