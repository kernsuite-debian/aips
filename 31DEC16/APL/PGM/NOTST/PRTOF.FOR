LOCAL INCLUDE 'PRTOF.INC'
      INCLUDE 'INCS:DOFV.INC'
      HOLLERITH XINNAM(3), XINCLS(2), XLPNAM(12)
      REAL      XSEQ, XDISK, XVER, XBCNT, XECNT, XINC, DOCRT, X1, DOFLAG
      CHARACTER INNAM*12, INCLS*6, INEXT*2, LPNAME*48, ANNAME*8,
     *   OBSCOD*8, OBSDAT*8
      INTEGER   BCOUNT, ECOUNT, ICOUNT, SCRBUF(256), OFBUFF(512),
     *   INSEQ, INDISK, INVERS, CNO, IUSER, OUTLUN, OUTIND, PN1, NACROS,
     *   IOFRNO, OFKOLS(MAXOFC), OFNUMV(MAXOFC), TABVER, NPOL, NIF
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XBCNT, XECNT,
     *   XINC, DOCRT, XLPNAM, X1, DOFLAG
      COMMON /CHPARM/ INNAM, INCLS, INEXT, LPNAME, ANNAME, OBSCOD,
     *   OBSDAT
      COMMON /PRTOFC/ BCOUNT, ECOUNT, ICOUNT, SCRBUF, OFBUFF, INSEQ,
     *   INDISK, INVERS, CNO, IUSER, OUTLUN, OUTIND, PN1, NACROS,
     *   IOFRNO, OFKOLS, OFNUMV, TABVER, NPOL, NIF
LOCAL END
      PROGRAM PRTOF
C-----------------------------------------------------------------------
C! Task to print contents of on-line flag (OF) table extension files.
C# Calibration EXT-appl EXT-util VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 2008
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
C   PRTOF is an AIPS task to print the contents of on-line flag (OF)
C   table files on the line printer or the CRT terminal.
C   AIPS adverbs:                     Use:
C     INNAME     INNAM(3)     Image name: standard defaults, wildcards
C     INCLASS    INCLS(2)     Image class: ditto
C     INSEQ      XSEQ (INSEQ) Image sequence number: ditto
C     INDISK     XDISK (INDISK)  Image disk number: 0 -> any
C     INEXT      INEXT           Extension type: '  ' => 'TA'
C     INVERS     XVER (INVERS)   Extension version number
C     BCOUNT     XBCNT (BCOUNT)  First row number
C     ECOUNT     XECNT (ECOUNT)  Last row number
C     XINC       XINC (ICOUNT)   Increment between printed rows
C     DOCRT      DOCRT           > 0. => CRT, else line printer
C     OUTPRINT   LPNAME          File to save printer output in
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTOF.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL PRTOIN (IRET)
C                                       do printing
      IF (IRET.EQ.0) CALL PRTODO (IRET)
C                                       close down
      CALL DIE (IRET, OFBUFF)
C
 999  STOP
      END
      SUBROUTINE PRTOIN (IRET)
C-----------------------------------------------------------------------
C   PRTOIN performs initialization for AIPS task PRTOF.  It gets the
C   adverbs, opens the catalog file for 'READ', opens the table
C   extension
C   file, and opens the output device.
C   Output: IRET    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      CHARACTER INTYP*2, STAT*4, PRGN*6
      INTEGER   IRET, I4T, NPARM, IROUND, TABLUN, IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTOF.INC'
      DATA TABLUN, INTYP /27, 'UV'/
      DATA PRGN /'PRTOF '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 26
      IRET = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       restart AIPS
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      RQUICK = (RQUICK) .AND. (DOCRT.LE.0.0)
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      INEXT = 'OF'
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
C                                       find image file
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      ICOUNT = IROUND (XINC)
      BCOUNT = IROUND (XBCNT)
      ECOUNT = IROUND (XECNT)
      IF (ICOUNT.LE.0) ICOUNT = 1
      IF (BCOUNT.LE.0) BCOUNT = 1
      IUSER = NLUSER
      PN1 = IROUND (X1)
      IF (PN1.LE.0) PN1 = 100000
C                                       find data set
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file read
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'REST', SCRBUF, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Open table file
      CALL OFINI ('READ', OFBUFF, INDISK, CNO, INVERS, CATBLK, TABLUN,
     *   IOFRNO, OFKOLS, OFNUMV, NIF, NPOL, ANNAME, OBSCOD, OBSDAT,
     *   TABVER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, INEXT, INVERS
         GO TO 990
         END IF
      IF (TABVER.NE.2) THEN
         WRITE (MSGTXT,1035) TABVER
         IERR = 10
         GO TO 990
         END IF
      PN1 = MIN (PN1, NIF)
      I4T = OFBUFF(5)
      IF (BCOUNT.GT.I4T) BCOUNT = 1
      IF ((ECOUNT.LT.BCOUNT) .OR. (ECOUNT.GT.I4T)) ECOUNT = I4T
C                                       open printer
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, OUTLUN, OUTIND, NACROS, OFBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING INPUT ADVERBS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
 1020 FORMAT ('ERROR',I5,' READING CATBLK FROM CATALOG FILE')
 1030 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1035 FORMAT ('OF TABLE FORMAT VERSION',I4,' NOT RECOGNIZED')
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
      END
      SUBROUTINE PRTODO (IRET)
C-----------------------------------------------------------------------
C   PRTODO reads, formats, and prints an OF table extension file
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PRTOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRNO, IMIN(6), IMAX(6), SOURCE, IANT, SUBID, FREQID,
     *   ANFLAG, STATUS(2,MAXIF), LB, LP, LIF, BITS(32), NB, I, AP,
     *   ICH(MAXOFC+2), NCH(MAXOFC+2), M, JTRIM, BMAX, NLINE, IPAGE,
     *   HMS(3), NS, SP
      REAL      TMIN, TMAX, TIME, TEMP, SEC
      CHARACTER TITL1*132, TITL2*132, LINE*132, SCRTCH*132, CTEMP1*132,
     *   ATIME*8, ADATE*12, BTIME*8, BDATE*12, CHSIGN*1, VSYM(32)*8
      INCLUDE 'INCS:DMSG.INC'
      DATA VSYM /'RefPoint', 'Shadowed', 'OffSourc', '1 LOLock',
     *   'TsysFluc', 'Operator', 'BETotPow', 'BEFilter', 'L8Module',
     *   'L6Module', 'SubRefle', 'SouChang', 'PhSwitch', 'RndTrip',
     *   18*'????????'/
C-----------------------------------------------------------------------
C                                       find max min
      TMIN = 1.E10
      TMAX = -TMIN
      DO 10 I = 1,6
         IMAX(I) = -1000000
         IMIN(I) = -IMAX(I)
 10      CONTINUE
      BMAX = 0
      DO 30 IRNO = BCOUNT,ECOUNT,ICOUNT
         IOFRNO = IRNO
         CALL TABOF ('READ', OFBUFF, IOFRNO, OFKOLS, OFNUMV, TIME,
     *      SOURCE, IANT, SUBID, FREQID, ANFLAG, STATUS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, IRNO
            GO TO 990
         ELSE IF ((IRET.EQ.0) .OR. (DOFLAG.GT.0.0)) THEN
            TMIN = MIN (TMIN, TIME)
            TMAX = MAX (TMAX, TIME)
            IMIN(1) = MIN (IMIN(1), SOURCE)
            IMIN(2) = MIN (IMIN(2), IANT)
            IMIN(3) = MIN (IMIN(3), SUBID)
            IMIN(4) = MIN (IMIN(4), FREQID)
            IMAX(1) = MAX (IMAX(1), SOURCE)
            IMAX(2) = MAX (IMAX(2), IANT)
            IMAX(3) = MAX (IMAX(3), SUBID)
            IMAX(4) = MAX (IMAX(4), FREQID)
            DO 20 LIF = 1,NIF
               DO 15 LP = 1,NPOL
                  IMIN(5) = MIN (IMIN(5), STATUS(LP,LIF))
                  IMAX(5) = MAX (IMAX(5), STATUS(LP,LIF))
 15               CONTINUE
 20            CONTINUE
            CALL ZGTBIT (32, ANFLAG, BITS)
            NB = 0
            DO 25 LB = 1,32
               NB = NB + BITS(LB)
               IF (BITS(LB).GT.0) BMAX = MAX (BMAX, LB)
 25            CONTINUE
            IMIN(6) = MIN (IMIN(6), NB)
            IMAX(6) = MAX (IMAX(6), NB)
            END IF
 30      CONTINUE
C                                       count up column positions
      NS = 1
 40   ICH(1) = 1
      IF (DOFLAG.GT.0.0) ICH(1) = 2
      TEMP = ECOUNT
      I = LOG10 (TEMP) + 1.001
      NCH(1) = MAX (I,4)
      ICH(2) = ICH(1) + NCH(1) + NS
      NCH(2) = LOG10 (TMAX) + 1.001
      IF (NCH(2).GT.0) THEN
         ICH(3) = ICH(2) + NCH(2) + 12 + NS
      ELSE
         ICH(3) = ICH(2) + 11 + NS
         END IF
      DO 45 LP = 1,4
         TEMP = IMAX(LP)
         I = LOG10 (TEMP) + 1.001
         NCH(2+LP) = MAX (I,3)
         ICH(3+LP) = ICH(2+LP) + NCH(2+LP) + NS
 45      CONTINUE
      IF (NIF.GT.1) THEN
         NCH(7) = 2
         ICH(8) = ICH(7) + 2 + NS
         SP = 8
      ELSE
         SP = 7
         END IF
      TEMP = IMAX(5)
      I = LOG10 (TEMP) + 1.001
      NCH(SP) = MAX (I,4)
      ICH(SP+1) = ICH(SP) + NCH(SP) + NS
      IF (NPOL.GT.1) THEN
         NCH(SP+1) = NCH(SP)
         ICH(SP+2) = ICH(SP+1) + NCH(SP+1) + NS
         AP = SP + 2
      ELSE
         AP = SP + 1
         END IF
      NCH(AP) = NACROS - ICH(AP) + 1
      IF (ANNAME.EQ.'VLA') THEN
         IMAX(6) = MAX (IMAX(6), 1)
         NB = NCH(AP) / IMAX(6)
         IF ((NB.GT.8+NS) .AND. (NS.LE.3)) THEN
            NS = NS + 1
            GO TO 40
            END IF
         NB = MIN (NB, 8+NS)
      ELSE
         BMAX = MAX (1, BMAX)
         NB = NCH(AP) - BMAX*2
         IF ((NB.GT.9) .AND. (NS.LE.3)) THEN
            NS = NS + 1
            GO TO 40
            END IF
         END IF
C                                       First page header
      NLINE = 999
      IPAGE = 0
      WRITE (TITL1,1040) INNAM, INCLS, INSEQ, INDISK, INEXT, INVERS
      TITL2 = 'Title:'
      CALL H2CHR (56, 1, OFBUFF(101), TITL2(9:))
      M = JTRIM (TITL2)
      IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL1, NLINE, IPAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
      ELSE
         CALL TIMDAT (OFBUFF(14), OFBUFF(11), ATIME, ADATE)
         CALL TIMDAT (OFBUFF(36), OFBUFF(33), BTIME, BDATE)
         CALL H2CHR (5, 1, OFBUFF(29), CTEMP1)
         M = JTRIM (CTEMP1)
         WRITE (LINE,1042) CTEMP1, ADATE, ATIME
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL H2CHR (5, 1, OFBUFF(39), CTEMP1)
         M = JTRIM (CTEMP1)
         WRITE (LINE,1043) CTEMP1, BDATE, BTIME
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (LINE,1044) ANNAME, OBSCOD, OBSDAT, NIF, NPOL
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         LINE = ' '
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       Build column titles
      TITL1 = ' '
      TITL2 = ' '
      I = ICH(1)+NCH(1) - 3
      TITL1(I:) = 'ROW'
      I = ICH(1)+NCH(1) - 6
      IF (I.GE.1) TITL2(I:) = 'NUMBER'
      I = ICH(3) - 7 - NS
      TITL1(I:) = 'TIME'
      TITL2(I-1:) = 'D/HMS'
      CTEMP1 = 'SOURCE'
      TITL1(ICH(3):) = CTEMP1(:NCH(3))
      CTEMP1 = 'ANTENNA'
      TITL1(ICH(4):) = CTEMP1(:NCH(4))
      CTEMP1 = 'SUBARRAY'
      TITL1(ICH(5):) = CTEMP1(:NCH(5))
      CTEMP1 = 'FREQID'
      TITL1(ICH(6):) = CTEMP1(:NCH(6))
      IF (NIF.GT.1) TITL1(ICH(7):) = 'IF'
      CTEMP1 = 'STATUS'
      TITL1(ICH(SP):) = CTEMP1(:NCH(SP))
      CTEMP1 = 'POL1'
      TITL2(ICH(SP):) = CTEMP1(:NCH(SP))
      IF (NPOL.GT.1) THEN
         CTEMP1 = 'STATUS'
         TITL1(ICH(SP+1):) = CTEMP1(:NCH(SP+1))
         CTEMP1 = 'POL2'
         TITL2(ICH(SP+1):) = CTEMP1(:NCH(SP+1))
         END IF
      TITL1(ICH(AP):) = 'Flag bits'
      TITL2(ICH(AP):) = 'by antenna'
      IF (ANNAME.NE.'VLA') WRITE (TITL2(ICH(AP):),1045) (I, I = 1,BMAX)
C                                       Output the titles
      CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *   TITL1, NLINE, IPAGE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *   TITL2, NLINE, IPAGE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 970
      IF ((DOCRT.GT.-2.5) .AND. (DOCRT.LE.0.0)) THEN
         LINE = ' '
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       loop over file
      DO 70 IRNO = BCOUNT,ECOUNT,ICOUNT
         IOFRNO = IRNO
         CALL TABOF ('READ', OFBUFF, IOFRNO, OFKOLS, OFNUMV, TIME,
     *      SOURCE, IANT, SUBID, FREQID, ANFLAG, STATUS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, IRNO
            GO TO 990
         ELSE IF ((IRET.EQ.0) .OR. (DOFLAG.GT.0.0)) THEN
            LINE = ' '
            IF (IRET.LT.0) LINE(1:1) = '*'
            WRITE (CTEMP1,1050) IRNO
            I = ICH(2) - NS - NCH(1)
            LINE(I:) = CTEMP1(13-NCH(1):12)
C                                       time is messy
            CALL TFDHMS (TIME, 1, CHSIGN, HMS, SEC)
            WRITE (CTEMP1,1051) HMS, SEC
            IF (CTEMP1(16:16).EQ.' ') CTEMP1(16:16) = '0'
            IF (CTEMP1(17:17).EQ.' ') CTEMP1(17:17) = '0'
            IF (NCH(2).GT.0) THEN
               IF (CHSIGN.EQ.'-') CTEMP1(9-NCH(2):9-NCH(2)) = '-'
               LINE(ICH(2):) = CTEMP1(9-NCH(2):20)
            ELSE
               LINE(ICH(2):) = CTEMP1(10:20)
               END IF
C                                       simple integers
            I = ICH(4) - NS - NCH(3)
            WRITE (CTEMP1,1050) SOURCE
            LINE(I:) = CTEMP1(13-NCH(3):)
            I = ICH(5) - NS - NCH(4)
            WRITE (CTEMP1,1050) IANT
            LINE(I:) = CTEMP1(13-NCH(4):)
            I = ICH(6) - NS - NCH(5)
            WRITE (CTEMP1,1050) SUBID
            LINE(I:) = CTEMP1(13-NCH(5):)
            I = ICH(7) - NS - NCH(6)
            WRITE (CTEMP1,1050) FREQID
            LINE(I:) = CTEMP1(13-NCH(6):)
            IF (NIF.GT.1) THEN
               I = ICH(8) - NS - NCH(7)
               WRITE (CTEMP1,1050) 1
               LINE(I:) = CTEMP1(13-NCH(7):)
               END IF
            IF (STATUS(1,1).NE.0) THEN
               I = ICH(SP+1) - NS - NCH(SP)
               WRITE (CTEMP1,1050) STATUS(1,1)
               LINE(I:) = CTEMP1(13-NCH(SP):)
               END IF
            IF (NPOL.GT.1) THEN
               IF (STATUS(2,1).NE.0) THEN
                  I = ICH(SP+2) - NS - NCH(SP+1)
                  WRITE (CTEMP1,1050) STATUS(2,1)
                  LINE(I:) = CTEMP1(13-NCH(SP+1):)
                  END IF
               END IF
C                                       bit pattern
            CALL ZGTBIT (32, ANFLAG, BITS)
            IF (ANNAME.NE.'VLA') THEN
               I = ICH(AP) + 1
               DO 50 LB = 1,BMAX
                  IF (BITS(LB).NE.0) LINE(I:I) = 'X'
                  I = I + 2
 50               CONTINUE
            ELSE
               I = ICH(AP)
               DO 55 LB = 1,BMAX
                  IF (BITS(LB).GT.0) THEN
                     LINE(I:) = VSYM(LB)(1:NB-NS)
                     I = I + NB
                     END IF
 55               CONTINUE
               END IF
C                                       do output finally
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 970
C                                       loop over other IFs
            DO 60 LIF = 2,PN1
               LINE = ' '
               IF (NIF.GT.1) THEN
                  I = ICH(8) - NS - NCH(7)
                  WRITE (CTEMP1,1050) LIF
                  LINE(I:) = CTEMP1(13-NCH(7):)
                  END IF
               IF (STATUS(1,LIF).NE.0) THEN
                  I = ICH(SP+1) - NS - NCH(SP)
                  WRITE (CTEMP1,1050) STATUS(1,LIF)
                  LINE(I:) = CTEMP1(13-NCH(SP):)
                  END IF
               IF (NPOL.GT.1) THEN
                  IF (STATUS(2,LIF).NE.0) THEN
                     I = ICH(SP+2) - NS - NCH(SP+1)
                     WRITE (CTEMP1,1050) STATUS(2,LIF)
                     LINE(I:) = CTEMP1(13-NCH(SP+1):)
                     END IF
                  END IF
               CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, NLINE, IPAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 970
 60            CONTINUE
            END IF
 70      CONTINUE
C                                       close down
 970  CALL TABOF ('CLOS', OFBUFF, IOFRNO, OFKOLS, OFNUMV, TIME, SOURCE,
     *   IANT, SUBID, FREQID, ANFLAG, STATUS, I)
      CALL LPCLOS (OUTLUN, OUTIND, NLINE, I)
C                                       error?
 990  IF (IRET.GT.0) CALL MSGWRT (8)
      IRET = MAX (0, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I5,' READING TABLE ROW',I10)
 1040 FORMAT (A12,'.',A6,'.',I4,'  Disk=',I2,4X,A2,' Table version',
     *   I4)
 1042 FORMAT ('Created by      ',A5,' on ',A12,A8)
 1043 FORMAT ('Last written by ',A5,' on ',A12,A8)
 1044 FORMAT ('Array ''',A,'''  ObsCode ''',A,'''  ObsDate ''',A,
     *   '''  NIF',I3,'  NPol',I2)
 1045 FORMAT (32I2)
 1050 FORMAT (I12)
 1051 FORMAT (I8.8,'/',I2.2,':',I2.2,':',F5.2)
      END
