LOCAL INCLUDE 'PCFIT.INC'
      REAL      XSEQ, XDISK, XVER, XFQ, XSUB, XTIME(8), XPIECE, PRTLEV
      HOLLERITH XINNAM(3), XINCLS(2), XOPTYP
      CHARACTER INNAM*12, INCLS*6, OPTYPE*4
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XFQ, XSUB,
     *   XTIME, XPIECE, XOPTYP, PRTLEV
      COMMON /CHPARM/ INNAM, INCLS, OPTYPE
      INTEGER   INSEQ, INDISK, INVERS, CNO, FQID, SUBARR, IFSTEP,
     *   SCRTCH(256), OUVERS
      DOUBLE PRECISION TIMBEG, TIMEND
      COMMON /PCFITC/ TIMBEG, TIMEND, INSEQ, INDISK, INVERS, CNO, FQID,
     *   SUBARR, IFSTEP, OUVERS, SCRTCH
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCHND.INC'
LOCAL END
LOCAL INCLUDE 'PCDATA.INC'
      INCLUDE 'INCS:PPCV.INC'
      DOUBLE PRECISION TIME, CABCAL, PCFREQ(2,MAXTON,MAXIF)
      INTEGER   PCNPOL, PCNIF, NUMTON, PCBUFF(512), PCNUMV(MAXPCC),
     *   PCKOLS(MAXPCC), PCRNO, SOUNUM, ANTNUM, ISUB, IDFREQ, NPCINR,
     *   PCBUFO(512)
      REAL TIMINT, STATE(2,4,MAXIF), PCREAL(2,MAXTON,MAXIF),
     *   PCIMAG(2,MAXTON,MAXIF), PCRATE(2,MAXTON,MAXIF)
      COMMON /PCDATA/ PCFREQ, TIME, CABCAL, STATE, PCREAL, PCIMAG,
     *   PCRATE, PCBUFF, PCBUFO, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON,
     *   PCRNO, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, NPCINR
LOCAL END
LOCAL INCLUDE 'PCFITD.INC'
      INCLUDE 'INCS:PPCV.INC'
      INTEGER   SPMAX
      PARAMETER (SPMAX = MAXTON*MAXIF)
C
      INTEGER   ITTER, NITTER
      DOUBLE PRECISION QDATA(SPMAX), UDATA(SPMAX), DELTAF(SPMAX)
      COMMON /PCFITD/ DELTAF, QDATA, UDATA, ITTER, NITTER
LOCAL END
LOCAL INCLUDE 'PCFITSN.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   GOTANT(2,MAXANT,MAXIF), REFANT(2,MAXIF)
      REAL      CREAL(2,MAXIF), CIMAG(2,MAXIF), CDELAY(2,MAXIF),
     *   CRATE(2,MAXIF), WEIGHT(2,MAXIF), DPHASE(2,MAXANT,MAXIF),
     *   DDELAY(2,MAXANT,MAXIF)
      COMMON /PCFITX/ DPHASE, DDELAY, GOTANT
LOCAL END
      PROGRAM PCFIT
C-----------------------------------------------------------------------
C! Fits pulse-cal table data for delays and phases
C# EXT-appl Calibration Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   Fits pulse-cal tables for delays and phases
C   Inputs from user
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      INVERS.....Specifies the version of the CP table to be read as
C                 input.   0 -> highest.
C                 The output version is always highest + 1.
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'PCFIT.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'PCFITD.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL PCFITI (IRET)
C                                       do plotting
      IF (IRET.EQ.0) CALL PCFITS (IRET)
C                                       close down
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE PCFITI (IRET)
C-----------------------------------------------------------------------
C   PCPLTI performs initialization for AIPS task PCFIT.  It gets the
C   adverbs, opens the catalog file for 'READ' (eventually), sorts and
C   opens the PC input file
C   Output:
C      IRET    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCFIT.INC'
      INCLUDE 'PCDATA.INC'
      CHARACTER INTYP*2, STAT*4, PRGN*6, KEYSPC(2)*24
      INTEGER   IROUND, PCLUN, JERR, KEY(2,2), NKEY, KOLS(2), J1, J2, I,
     *   KEYSUB(2,2), NPARMS, IUSER
      REAL      FKEY(2,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PCLUN, INTYP /27, 'UV'/
      DATA PRGN /'PCFIT '/
      DATA NKEY /2/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYSPC /'ANTENNA_NO', 'TIME '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARMS = 21
      IRET = 0
      CALL GTPARM (PRGN, NPARMS, RQUICK, XINNAM, SCRTCH, JERR)
      IF (JERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (JERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) JERR, 'GET INPUT ADVERBS'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, JERR)
      IF (IRET.NE.0) GO TO 999
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      IUSER = NLUSER
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file write
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'WRIT', SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ CATALOG HEADER'
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
      FRW(1) = 1
C                                       Open PC file
      CALL PCINI ('READ', PCBUFF, INDISK, CNO, INVERS, CATBLK, PCLUN,
     *   PCRNO, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT PC TABLE'
         GO TO 990
         END IF
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYSPC, 24, .TRUE., PCBUFF, KOLS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FIND PC COLUMNS'
         GO TO 990
         END IF
C                                       Sort to ant-time order
      J1 = 2
      J2 = 1
      IF ((PCBUFF(43).NE.KOLS(J1)) .OR. (PCBUFF(44).NE.KOLS(J2))) THEN
C                                       Close
         CALL TABIO ('CLOS', 0, PCRNO, PCBUFF, PCBUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE PC TABLE'
            GO TO 990
            END IF
C                                       sort
         KEY(1,1) = KOLS(J1)
         KEY(2,1) = KOLS(J1)
         KEY(1,2) = KOLS(J2)
         KEY(2,2) = KOLS(J2)
         CALL TABSRT (INDISK, CNO, 'PC', INVERS, INVERS, KEY, KEYSUB,
     *      FKEY, PCBUFF, CATBLK, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORT PC TABLE'
            GO TO 990
            END IF
C                                       Re-open PC table for read
         CALL PCINI ('READ', PCBUFF, INDISK, CNO, INVERS, CATBLK, PCLUN,
     *      PCRNO, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN SORTED PC TABLE'
            GO TO 990
            END IF
         END IF
      TIMBEG = ((XTIME(4)/60.0 + XTIME(3)) / 60.0 + XTIME(2)) / 24.0 +
     *   XTIME(1)
      TIMEND = ((XTIME(8)/60.0 + XTIME(7)) / 60.0 + XTIME(6)) / 24.0 +
     *   XTIME(5)
      IF (TIMEND.LE.TIMBEG) TIMEND = 1000.
      FQID = IROUND (XFQ)
      IF (FQID.LE.0) FQID = 1
      SUBARR = IROUND (XSUB)
      IF (SUBARR.LE.0) SUBARR = 1
      I = XPIECE + 0.01
      IF ((I.LE.0) .OR. (I.GT.PCNIF)) I = PCNIF
      IFSTEP = PCNIF / I
C                                       Open PC file
      OUVERS = 0
      CALL PCINI ('WRIT', PCBUFO, INDISK, CNO, OUVERS, CATBLK, PCLUN+1,
     *   PCRNO, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT PC TABLE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1020) OUVERS
      CALL MSGWRT (3)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCPLTI: ERROR',I5,' ON ',A)
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
 1020 FORMAT ('Writing output residual PC table version',I5)
      END
      SUBROUTINE PCFITS (IRET)
C-----------------------------------------------------------------------
C   Does the fitting on open PC table
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      EXTERNAL PCFUNC
      INTEGER   MAXPRM
      PARAMETER (MAXPRM = 2)
C
      INCLUDE 'PCFIT.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'PCFITD.INC'
      INCLUDE 'PCFITSN.INC'
C
      INTEGER   IROW, IPOL, ITC, IIF, I, SNVER, SNBUFF(512), ISNRNO,
     *   SNKOLS(MAXSNC), SNNUMV(MAXSNC), SNNUMA, SNNPOL, SNNIF, NUMNOD,
     *   SNLUN
      DOUBLE PRECISION TOL
      REAL      GMMOD, RANOD, DECNOD, IFR, MBDELY(2), DISP(2), DDISP(2),
     *   PCDELY(2,MAXIF), PCPHAS(2,MAXIF), ERDELY(2,MAXIF),
     *   ERPHAS(2,MAXIF)
      LOGICAL   ISAPPL
      CHARACTER KEYWRD*8
      INCLUDE 'INCS:PSTD.INC'
      DATA SNLUN /23/
      DATA IFR, MBDELY, DISP, DDISP /7*0.0/
C-----------------------------------------------------------------------
C                                       init for SN table
      I = 2 * PCNIF
      CALL RFILL (I, 0.0, CRATE)
      CALL FILL (I, 0, REFANT)
      CALL FILL (2*MAXANT*MAXIF, 0, GOTANT)
      SNVER = 0
      SNNUMA = 0
      SNNPOL = PCNPOL
      SNNIF = PCNIF
      NUMNOD = 0
      GMMOD = 1.0
      RANOD = 0.0
      DECNOD = 0.0
      ISAPPL = .FALSE.
      CALL SNINI ('WRIT', SNBUFF, INDISK, CNO, SNVER, CATBLK, SNLUN,
     *   ISNRNO, SNKOLS, SNNUMV, SNNUMA, SNNPOL, SNNIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING NEW SN TABLE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1001) SNVER
      CALL MSGWRT (3)
C                                       read through PC table
      NITTER = 100
      NPCINR = PCBUFF(5)
      ITC  = (NUMTON  + 1) / 2
      DO 100 IROW = 1,NPCINR
         PCRNO = IROW
         CALL TABPC ('READ', PCBUFF, PCRNO, PCKOLS, PCNUMV, PCNPOL,
     *      TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, CABCAL,
     *      STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING PC TABLE'
            GO TO 990
            END IF
         IF (IRET.LT.0) GO TO 100
         SNNUMA = MAX (SNNUMA, ANTNUM)
         WRITE (MSGTXT,1010) IROW, ANTNUM
         IF (PRTLEV.GT.0.0) CALL MSGWRT (2)
         IF (CABCAL.EQ.DBLANK) CABCAL = 0.0D0
         IF (CABCAL.EQ.FBLANK) CABCAL = 0.0D0
         CABCAL = CABCAL * 1.0D-9
C                                       get solution and residual
         CALL PCFITR (PCNPOL, PCNIF, IFSTEP, NUMTON, PCFREQ, PCREAL,
     *      PCIMAG, PRTLEV, PCDELY, PCPHAS, ERDELY, ERPHAS, WEIGHT,
     *      IRET)
         IRET = 0
         DO 90 IPOL = 1,PCNPOL
            DO 80 IIF = 1,PCNIF
               IF (WEIGHT(IPOL,IIF).GT.0.0) THEN
                  IF (GOTANT(IPOL,ANTNUM,IIF).LE.0) THEN
                     DPHASE(IPOL,ANTNUM,IIF) = PCPHAS(IPOL,IIF)
                     DDELAY(IPOL,ANTNUM,IIF) = PCDELY(IPOL,IIF) -
     *                  CABCAL
                     GOTANT(IPOL,ANTNUM,IIF) = 1
                     END IF
                  TOL = PCPHAS(IPOL,IIF) - DPHASE(IPOL,ANTNUM,IIF)
                  CREAL(IPOL,IIF) = COS (TOL)
                  CIMAG(IPOL,IIF) = SIN (TOL)
                  IF (OPTYPE.EQ.'TOTA') THEN
                     CDELAY(IPOL,IIF) = PCDELY(IPOL,IIF)
                  ELSE
                     CDELAY(IPOL,IIF) = (PCDELY(IPOL,IIF) -
     *                  DDELAY(IPOL,ANTNUM,IIF))
                     END IF
               ELSE
                  CREAL(IPOL,IIF) = FBLANK
                  CIMAG(IPOL,IIF) = FBLANK
                  CDELAY(IPOL,IIF) = FBLANK
                  END IF
 80            CONTINUE
 90         CONTINUE
C                                       write SN table record
         CALL TABSN ('WRIT', SNBUFF, ISNRNO, SNKOLS, SNNUMV, SNNPOL,
     *      TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, IFR, NUMNOD,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, CDELAY, CRATE, WEIGHT,
     *      REFANT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING SN TABLE RECORD'
            GO TO 990
            END IF
         PCRNO = IROW
         CALL TABPC ('WRIT', PCBUFO, PCRNO, PCKOLS, PCNUMV, PCNPOL,
     *      TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, CABCAL,
     *      STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT PC TABLE'
            GO TO 990
            END IF
 100     CONTINUE
C                                       fix SN table number antennas
      KEYWRD = 'NO_ANT'
      CALL TABKEY ('WRIT', KEYWRD, 1, SNBUFF, 1, SNNUMA, 4, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATING SN TABLE KEYWORD'
         CALL MSGWRT (7)
         IRET = 0
         END IF
C                                       close tables
      CALL TABIO ('CLOS', 0, PCRNO, PCBUFF, PCBUFF, I)
      CALL TABIO ('CLOS', 0, PCRNO, PCBUFO, PCBUFO, I)
      CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' ON ',A)
 1001 FORMAT ('Writing SN table version',I5)
 1010 FORMAT ('Fitting row',I5,' antenna',I4)
      END
