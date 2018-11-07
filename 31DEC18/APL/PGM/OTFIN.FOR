LOCAL INCLUDE 'OTFIN.INC'
C                                       Local include for OTFIN
C                                       inputs
      HOLLERITH XINFIL(12), XPRINT(12)
      REAL      XBCNT, XECNT, XBIF, DOCRT
      COMMON /INPARM/ XINFIL, XBCNT, XECNT, XBIF, DOCRT, XPRINT
C                                       internals
      INTEGER   SCRBUF(256), BCOUNT, ECOUNT, BIF, LUNP, FINDP, NACROS,
     *   PAGE
      CHARACTER INFILE*48, TITL1*132, TITL2*132, SCRTCH*132, LINE*132,
     *   LPNAME*48
      COMMON /OTFINP/ SCRBUF, BCOUNT, ECOUNT, BIF, LUNP, FINDP, NACROS,
     *   PAGE
      COMMON /OTFINC/ INFILE, TITL1, TITL2, SCRTCH, LINE, LPNAME
C                                       SDD IO package
      DOUBLE PRECISION HEADS(192), JD0, JD
      INTEGER   IOBUF(256), ILUN, IIND, IIRET, IIREC, MAXREC, NCHAN,
     *   IHEADS(384)
      LOGICAL   SWAPED
      EQUIVALENCE (HEADS, IHEADS)
      COMMON /SDDIO/ HEADS, IOBUF, JD0, JD, ILUN, IIND, IIRET, IIREC,
     *   MAXREC, NCHAN, SWAPED
      INCLUDE 'INCS:DSDD.INC'
C                                       standard includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
      PROGRAM OTFIN
C-----------------------------------------------------------------------
C! Translates on-the-fly single-dish SDD format to index the contents
C# Task General Singledish Hardcopy
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997, 1999, 2004-2005, 2007, 2015
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
C   Task to read singledish data in UniPOPS internal format (SDD) and
C   write a printer summary of the contents
C   Limitations:
C      Now only does OTF line data from the 12m
C   Input adverbs:
C      INFILE......Input 12m sdd file (raw data file).
C      BCOUNT......First scan in OTF map which you want to process.
C      ECOUNT......Last scan in OTF map which you want to process.
C      BIF.........IF number to process.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'OTFIN.INC'
      DATA PRGM /'OTFIN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output print file
      CALL OTFINI (PRGM, IRET)
C                                       write the file
      CALL OTFINF (IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE OTFINI (PRGM, IRET)
C-----------------------------------------------------------------------
C   gets input parameters for OTFIN
C   Inputs:
C      PRGM    C*6   Program name
C   Output:
C      IRET    I     Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      INTEGER   NPARM, IERR, IROUND
      INCLUDE 'OTFIN.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      SWAPED = .FALSE.
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 28
      CALL GTPARM (PRGM, NPARM, RQUICK, XINFIL, SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      RQUICK = (RQUICK) .AND. (DOCRT.LE.0.0)
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Crunch input parameters.
      BCOUNT = IROUND (XBCNT)
      IF (BCOUNT.LE.0) BCOUNT = 1
      ECOUNT = IROUND (XECNT)
      IF (ECOUNT.LT.BCOUNT) ECOUNT = 10000000
      BIF = IROUND (XBIF)
      IF (BIF.LE.0) BIF = 0
C                                       Characters
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XPRINT, LPNAME)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFINI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
      END
      SUBROUTINE OTFINF (IRET)
C-----------------------------------------------------------------------
C   Reads the inpuut SDD data and writes the summary information
C   In/Out:
C      IRET     I     Error code: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NREC, J, IBUF(128), MAXDIR, ID, IR, LS, LIF, II, JJ,
     *   ISC, IT(6), IDR, HM(2), DD(2), IPCNT, JTRIM, IP, OFINC, NWC,
     *   OFFSCN, IBYTFL
      CHARACTER SCNTYP*8, CHSIG1*1, CHSIG2*1, OBJECT*8, LDATE*12,
     *   CDATE*12
      LOGICAL   T, F, FIRST, LINUXS
      REAL      RBUF(128), RTEMP, RSEC, DSEC
      DOUBLE PRECISION DTEMP, ORA, ODE, OFREQ, VEL
      INCLUDE 'OTFIN.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NWC = 4096 / NBITWD / NWDPDP
      IF (IRET.NE.0) GO TO 999
      FIRST = .TRUE.
C                                       Init the SDD IO
      ILUN = 17
      IIREC = -1
      MAXREC = -1
      LDATE = ' '
C                                       open data and gains files
      CALL ZOPEN (ILUN, IIND, 1, INFILE, F, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'DATA'
         GO TO 990
         END IF
      SWAPED = .FALSE.
      IBYTFL = BYTFLP
      CALL FILL (3, 0, IT(4))
C                                       loop through the directory
C                                       read and check the bootstrap
      IDR = 1
      CALL OTFINO (IDR, IBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Swapped bytes ??
      IF ((IBUF(3).EQ.512) .OR. (IBUF(4).EQ.64)) THEN
         LINUXS = (IBYTFL.EQ.3)
         IF (LINUXS) BYTFLP = 3 - BYTFLP
      ELSE
         LINUXS = (IBYTFL.EQ.0)
         IF (LINUXS) BYTFLP = 3 - BYTFLP
         CALL ZI32IL (128, 1, IBUF, IBUF)
         SWAPED = (IBUF(3).EQ.512) .AND. (IBUF(4).EQ.64)
         IF (.NOT.SWAPED) CALL ZILI32 (128, IBUF, 1, IBUF)
         END IF
      IF ((IBUF(3).NE.512) .OR. (IBUF(4).NE.64)) THEN
         WRITE (MSGTXT,1010) IBUF(3), IBUF(4)
         IRET = 1
         GO TO 990
         END IF
      MAXDIR = IBUF(5)
      IF ((IBUF(7).NE.0) .OR. (IBUF(8).NE.1)) THEN
         WRITE (MSGTXT,1011) IBUF(7), IBUF(8)
         CALL MSGWRT (6)
         END IF
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1015) IRET
         CALL MSGWRT (8)
         GO TO 960
         END IF
C                                      first page
      J = JTRIM (ISORT)
      PAGE = 0
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      LINE = 'OTF file ''' // INFILE
      J = JTRIM (LINE) + 1
      LINE (J:) = ''''
      J = J + 4
      IF ((BCOUNT.GT.1) .OR. (ECOUNT.LT.1000000)) THEN
         WRITE (LINE(J:),1020) BCOUNT, ECOUNT
      ELSE
         LINE(J:) = 'All scans'
         END IF
      J = JTRIM (LINE)
      J = J + 4
      IF (BIF.GT.0) THEN
         WRITE (LINE(J:),1021) BIF
      ELSE
         LINE(J:) = 'All IFs'
         END IF
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 950
      IF (NACROS.GT.128) THEN
         TITL1 = '  Scan  IF  Type       Chan   Samp     LST       Time'
     *      // '     Object        Freq     Finc Velocity      RA'
     *      // '           Dec         OFF'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
      ELSE IF (NACROS.GT.114) THEN
         TITL1 = '  Scan IF Type      Chan  Samp    LST      Time   '
     *      // ' Object       Freq    Finc Velocity    RA          '
     *      // 'Dec        OFF'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
      ELSE IF (NACROS.GE.80) THEN
         TITL1 = '  Scan IF Type       Samp    LST      Time   '
     *      // ' Object       Freq    Finc Velocity'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
         TITL2(49:) = 'RA          Dec      Chan   OFF'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
      ELSE
         TITL1 = '  Scan IF Type       Samp    LST      Time   '
     *      // ' Object       Freq    Finc'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
         TITL2(33:) = 'RA          Dec     Velocity Chan   OFF'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
         END IF
C                                       read directory
      DO 40 ID = 1,MAXDIR,8
         IDR = IDR + 1
         CALL OTFINO (IDR, IBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (128, IBUF, RBUF)
         IF (SWAPED) THEN
            CALL ZI32IL (128, 1, IBUF, IBUF)
            CALL ZBFLIP (4, 128, RBUF, RBUF)
            END IF
         DO 30 J = 1,8
            IF (ID+J-1.LE.MAXDIR) THEN
               RTEMP = RBUF(9+16*(J-1))
               LS = RTEMP + 0.001
               LIF = 100.0 * (RTEMP - LS) + 0.1
C                                       read the header
               IF (((LIF.EQ.BIF) .OR. (BIF.EQ.0)) .AND. (LS.GE.BCOUNT)
     *            .AND. (LS.LE.ECOUNT)) THEN
                  ISC = LS-BCOUNT+1
                  IR = IBUF(16*J-15)
                  DO 20 II = 1,3
                     JJ = NWC * (II - 1) + 1
                     JJ = 2 * JJ - 1
                     CALL OTFINO (IR, IHEADS(JJ), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1015) 'DATA HEADER', ISC
                        GO TO 990
                        END IF
                     IR = IR + 1
 20                  CONTINUE
                  IF (FIRST) CALL SDDPTS (IBYTFL, HEADS)
                  IF (SWAPED) CALL ZBFLIP (8, 192, HEADS, HEADS)
                  FIRST = .FALSE.
                  CALL DL2CHR (HEADS(SOBSM), SWAPED, SCNTYP)
                  IR = IBUF(16*J-15)
C                                       OTF on data
                  IF (SCNTYP.EQ.'LINEOTF ') THEN
                     SCNTYP = 'LINE OTF'
C                                       OTF on data
                  ELSE IF (SCNTYP.EQ.'LINETPMF') THEN
                     SCNTYP = 'LINE OFF'
                     END IF
C                                       Observing date.
C                                       get time
                  DTEMP = HEADS(SUTDA)
                  IT(1) = DTEMP
                  DTEMP = 100.0D0 * (DTEMP - IT(1))
                  IT(2) = DTEMP
                  DTEMP = 100.0D0 * (DTEMP - IT(2))
                  IT(3) = DTEMP + 0.1
                  WRITE (CDATE,1025) IT(1), IT(2), IT(3)
                  DTEMP = HEADS(SUTDA+1)
                  IT(4) = DTEMP
                  DTEMP = (DTEMP - IT(4)) * 60.0D0
                  IT(5) = DTEMP
                  DTEMP = (DTEMP - IT(5)) * 60.0D0
                  IT(6) = DTEMP + 0.5D0
                  CALL DAT2JD (IT, JD)
                  IF (JD0.LE.0.0D0) JD0 = JD
C                                       LST
                  DTEMP = HEADS(SUTDA+2)
                  IT(1) = DTEMP
                  DTEMP = (DTEMP - IT(1)) * 60.0D0
                  IT(2) = DTEMP
                  DTEMP = (DTEMP - IT(2)) * 60.0D0
                  IT(3) = DTEMP + 0.5D0
C                                       number samples
                  NREC = HEADS(SDLEN) + 0.1
                  NREC = NREC / 4
                  IF (SCNTYP(:4).EQ.'LINE') THEN
                     NCHAN = HEADS(SNOIN) + 0.1
                     IF (SCNTYP.EQ.'LINE OTF') THEN
                        MAXREC = NREC / (NCHAN + 5)
                     ELSE
                        MAXREC = NREC / NCHAN
                        END IF
                  ELSE
                     MAXREC = HEADS(SNOIN) + 0.1
                     IF (SCNTYP.EQ.'CONTOTF') THEN
                        SCNTYP = 'CONT OTF'
                        MAXREC = MAXREC / 9
                        NCHAN = 1
                     ELSE
                        NCHAN = NREC / MAXREC
                        END IF
                     END IF
                  IF (CDATE.NE.LDATE) THEN
                     LINE = 'Date of observation = ' // CDATE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 960
                     LDATE = CDATE
                     END IF
C                                       scan number
                  IP = HEADS(SSCAN) + 0.01
                  OFFSCN = HEADS(SOFFS) + 0.01
                  CALL DL2CHR (HEADS(SOBJE), SWAPED, OBJECT)
C                                       "Old" (observed) position.
                  ORA = HEADS(SXSOU)
                  ODE = HEADS(SYSOU)
                  OFREQ = HEADS(SOBSF)
                  OFINC = HEADS(SFRQR) * 1.0D3 + 0.5
                  VEL = HEADS(SX0)
                  CALL COORDD (1, ORA, CHSIG1, HM, RSEC)
                  CALL COORDD (2, ODE, CHSIG2, DD, DSEC)
                  IF (NACROS.GT.128) THEN
                     WRITE (LINE,1030) IP, LIF, SCNTYP, NCHAN, MAXREC,
     *                  IT, OBJECT, OFREQ, OFINC, VEL, CHSIG1, HM, RSEC,
     *                  CHSIG2, DD, DSEC, OFFSCN
                     IF (LINE(90:92).EQ.'  .') LINE(91:91) = '0'
                     IF (LINE(90:92).EQ.' -.') LINE(90:91) = '-0'
                     IF (LINE(104:104).EQ.' ') LINE(104:104) = '0'
                     IF (LINE(118:118).EQ.' ') LINE(118:118) = '0'
                     IF (LINE(105:105).EQ.' ') LINE(105:105) = '0'
                     IF (LINE(119:119).EQ.' ') LINE(119:119) = '0'
                  ELSE IF (NACROS.GT.114) THEN
                     WRITE (LINE,1031) IP, LIF, SCNTYP, NCHAN, MAXREC,
     *                  IT, OBJECT, OFREQ, OFINC, VEL, CHSIG1, HM, RSEC,
     *                  CHSIG2, DD, DSEC, OFFSCN
                     IF (LINE(80:82).EQ.'  .') LINE(81:81) = '0'
                     IF (LINE(80:82).EQ.' -.') LINE(80:81) = '-0'
                     IF (LINE(93:93).EQ.' ') LINE(93:93) = '0'
                     IF (LINE(94:94).EQ.' ') LINE(94:94) = '0'
                     IF (LINE(106:106).EQ.' ') LINE(106:106) = '0'
                     IF (LINE(107:107).EQ.' ') LINE(107:107) = '0'
                  ELSE IF (NACROS.GE.80) THEN
                     WRITE (LINE,1040) IP, LIF, SCNTYP, MAXREC,
     *                  IT, OBJECT, OFREQ, OFINC, VEL
                     IF (LINE(75:77).EQ.'  .') LINE(76:76) = '0'
                     IF (LINE(75:77).EQ.' -.') LINE(75:76) = '-0'
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 960
                     WRITE (LINE,1041) CHSIG1, HM, RSEC, CHSIG2, DD,
     *                  DSEC, NCHAN, OFFSCN
                     IF (LINE(52:52).EQ.' ') LINE(52:52) = '0'
                     IF (LINE(65:65).EQ.' ') LINE(65:65) = '0'
                     IF (LINE(53:53).EQ.' ') LINE(53:53) = '0'
                     IF (LINE(66:66).EQ.' ') LINE(66:66) = '0'
                  ELSE
                     WRITE (LINE,1042) IP, LIF, SCNTYP, MAXREC,
     *                  IT, OBJECT, OFREQ, OFINC
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 960
                     WRITE (LINE,1043) CHSIG1, HM, RSEC, CHSIG2, DD,
     *                  DSEC, VEL, NCHAN, OFFSCN
                     IF (LINE(36:36).EQ.' ') LINE(36:36) = '0'
                     IF (LINE(49:49).EQ.' ') LINE(49:49) = '0'
                     IF (LINE(37:37).EQ.' ') LINE(37:37) = '0'
                     IF (LINE(50:50).EQ.' ') LINE(50:50) = '0'
                     IF (LINE(56:58).EQ.'  .') LINE(57:57) = '0'
                     IF (LINE(56:58).EQ.' -.') LINE(56:57) = '-0'
                     END IF
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               END IF
 30         CONTINUE
 40      CONTINUE
C
      GO TO 960
C                                       CRT error
 950  CONTINUE
         WRITE (MSGTXT,1950) IRET
         CALL MSGWRT (8)
 960  IF (IRET.LE.0) IRET = 0
      CALL LPCLOS (LUNP, FINDP, IPCNT, IRET)
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFINF: ERROR',I4,1X,A,'ING ',A,' FILE')
 1010 FORMAT ('OTFINF: RECORD, DIR LENGTHS',2I5,' NOT 512 64')
 1011 FORMAT ('OTFINF: Type, Version',2I5,' not 0 1 - continuing')
 1015 FORMAT ('OTFINF: ERROR',I5,' OPENING PRINTER')
 1020 FORMAT ('Scans',I5,'-',I6)
 1021 FORMAT ('IF =',I3)
 1025 FORMAT (I6.2,'/',I2.2,'/',I2.2)
 1030 FORMAT (I6,I4,2X,A8,1X,I6,I7,I6.2,2(':',I2.2),I4.2,2(':',I2.2),2X,
     *   A8,1X,F10.2,I8,F9.2,2X,A1,2(I2.2,':'),F5.2,2X,A1,2(I2.2,':'),
     *   F4.1,I7)
 1031 FORMAT (I6,I3,1X,A8,1X,I5,I6,I5.2,2(':',I2.2),I3.2,2(':',I2.2),1X,
     *   A8,1X,F9.2,I7,F8.2,1X,A1,2(I2.2,':'),F5.2,1X,A1,2(I2.2,':'),
     *   F4.1,I6)
 1040 FORMAT (I6,I3,1X,A8,1X,I6,I5.2,2(':',I2.2),I3.2,2(':',I2.2),1X,A8,
     *   1X,F9.2,I7,F8.2)
 1041 FORMAT (44X,A1,2(I2.2,':'),F5.2,1X,A1,2(I2.2,':'),F4.1,I5,I6)
 1042 FORMAT (I6,I3,1X,A8,1X,I6,I5.2,2(':',I2.2),I3.2,2(':',I2.2),1X,A8,
     *   1X,F9.2,I7)
 1043 FORMAT (28X,A1,2(I2.2,':'),F5.2,1X,A1,2(I2.2,':'),F4.1,F8.2,I5,I6)
 1950 FORMAT ('ERROR',I5,' DOING I/O TO TERMINAL OR PRINTER')
      END
      SUBROUTINE OTFINO (NR, REC, IRET)
C-----------------------------------------------------------------------
C   does actual read IO from disk file
C   Inputs:
C      NR     I        Desired record number (512-byte records)
C   Outputs:
C      REC    I(128)   Data record
C      IRET   I        I/O error: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   NR, REC(128), IRET
C
      INTEGER   IP, IR
      CHARACTER FILET(2)*4
      INCLUDE 'OTFIN.INC'
      DATA FILET /'DATA','GAIN'/
C-----------------------------------------------------------------------
      IRET = -1
C                                       ZFIO record number
      IF (NR.GT.0) THEN
         IR = (NR-1)/2 + 1
C                                       don't have it yet
         IF (IR.NE.IIREC) THEN
            IIREC = IR
            CALL ZFIO ('READ', ILUN, IIND, IIREC, IOBUF, IIRET)
            IRET = IIRET
            IF ((IRET.NE.0) .AND. (IRET.LT.1128)) THEN
               WRITE (MSGTXT,1000) IRET, FILET, NR
               GO TO 990
               END IF
            END IF
C                                       copy the data
         IP = 1 + 128 * MOD (NR-1, 2)
         CALL COPY (128, IOBUF(IP), REC)
         IRET = 0
         IF ((IP.GT.1) .AND. (IIRET.GT.1000) .AND. (IIRET.LT.1256)) THEN
            IRET = 4
            WRITE (MSGTXT,1010) NR
            GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFINO: ERROR',I5,' READING SDD ',A,' FILE RECORD',I8)
 1010 FORMAT ('OTFINO: ATTEMPT TO READ PAST END-OF-FILE RECORD',I8)
      END
      SUBROUTINE DL2CHR (DT, SWAPED, STR)
C-----------------------------------------------------------------------
C   translates a double precision (local form) to character
C   Inputs:
C      DT       D     Double precision in local form
C      SWAPED   L     Are we byte swapped?
C   Output:
C      STR      C*8   Character form
C-----------------------------------------------------------------------
      DOUBLE PRECISION DT
      LOGICAL   SWAPED
      CHARACTER STR*8
C
      DOUBLE PRECISION TD
      HOLLERITH HT(2)
      EQUIVALENCE (TD, HT)
C-----------------------------------------------------------------------
      TD = DT
      IF (SWAPED) CALL ZBFLIP (8, 1, TD, TD)
      CALL H2CHR (8, 1, HT, STR)
C
 999  RETURN
      END
