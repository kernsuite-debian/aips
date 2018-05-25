LOCAL INCLUDE 'PCFIT.INC'
      REAL      XSEQ, XDISK, XVER, XFQ, XSUB, XTIME(8), XSOLIN, XPIECE,
     *   CUTOFF, PRTLEV
      HOLLERITH XINNAM(3), XINCLS(2), XOPTYP
      CHARACTER INNAM*12, INCLS*6, OPTYPE*4
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XFQ, XSUB,
     *   XTIME, XSOLIN, XPIECE, XOPTYP, CUTOFF, PRTLEV
      COMMON /CHPARM/ INNAM, INCLS, OPTYPE
      INTEGER   INSEQ, INDISK, INVERS, CNO, FQID, SUBARR, IFSTEP,
     *   SCRTCH(256), OUVERS
      DOUBLE PRECISION TIMBEG, TIMEND
      LOGICAL   DOCUT
      COMMON /PCFITC/ TIMBEG, TIMEND, INSEQ, INDISK, INVERS, CNO, FQID,
     *   SUBARR, IFSTEP, OUVERS, DOCUT, SCRTCH
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
C;  Copyright (C) 2016-2017
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
      IF (IRET.EQ.0) THEN
         IF (XSOLIN.EQ.0.0) THEN
            CALL PCFITS (IRET)
         ELSE
            CALL PCAVGS (IRET)
            END IF
         END IF
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
      NPARMS = 23
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
      IF (CUTOFF.EQ.0.0) CUTOFF = 100.0
      DOCUT = (OPTYPE.NE.'TOTA') .AND. (CUTOFF.GT.0.0)
      CUTOFF = CUTOFF / 1.0E9
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
      IF (XSOLIN.EQ.0.0) THEN
         J1 = 2
         J2 = 1
      ELSE
         J1 = 1
         J2 = 2
         END IF
      IF ((PCBUFF(43).NE.KOLS(J1)) .OR. (PCBUFF(44).NE.KOLS(J2))) THEN
C                                       Close
         CALL TABIO ('CLOS', 0, PCRNO, PCBUFF, PCBUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE PC TABLE'
            GO TO 990
            END IF
C                                       sort
         MSGTXT = 'Sorting the input table'
         CALL MSGWRT (2)
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
      PCBUFO(43) = PCBUFF(43)
      PCBUFO(44) = PCBUFF(44)
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
     *   SNLUN, ITT(4)
      DOUBLE PRECISION TOL
      REAL      GMMOD, RANOD, DECNOD, IFR, MBDELY(2), DISP(2), DDISP(2),
     *   XDAY, PCDELY(2,MAXIF), PCPHAS(2,MAXIF), ERDELY(2,MAXIF),
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
C                                       reverse sign
                  CREAL(IPOL,IIF) = COS (TOL)
                  CIMAG(IPOL,IIF) = -SIN (TOL)
                  IF (OPTYPE.EQ.'TOTA') THEN
                     CDELAY(IPOL,IIF) = -PCDELY(IPOL,IIF)
                  ELSE
                     CDELAY(IPOL,IIF) = -(PCDELY(IPOL,IIF) -
     *                  DDELAY(IPOL,ANTNUM,IIF))
                     IF (DOCUT) THEN
                        IF (ABS(CDELAY(IPOL,IIF)).GT.CUTOFF) THEN
                           WRITE (MSGTXT,1080) IPOL, ANTNUM, IIF
                           CALL MSGWRT (7)
                           XDAY = TIME
                           CALL TODHMS (XDAY, ITT)
                           WRITE (MSGTXT,1081)
     *                        DDELAY(IPOL,ANTNUM,IIF)*1.E9,
     *                        PCDELY(IPOL,IIF)*1.E9, ITT
                           CALL MSGWRT (7)
                           DPHASE(IPOL,ANTNUM,IIF) = PCPHAS(IPOL,IIF)
                           DDELAY(IPOL,ANTNUM,IIF) = PCDELY(IPOL,IIF) -
     *                        CABCAL
                           CDELAY(IPOL,IIF) = -(PCDELY(IPOL,IIF) -
     *                        DDELAY(IPOL,ANTNUM,IIF))
                           CREAL(IPOL,IIF) = 1.0
                           CIMAG(IPOL,IIF) = 0.0
                           END IF
                        END IF
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
 1080 FORMAT ('Polarization',I2,' antenna',I4,' IF',I3,
     *   ' zero pt delay changed')
 1081 FORMAT (5X,'from',F9.2,' to',F9.2,' at',I3,'/',2(I2.2,':'),I2.2)
      END
      SUBROUTINE PCAVGS (IRET)
C-----------------------------------------------------------------------
C   Does the fitting on open PC table with time averaging
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
     *   SNLUN, IR1, IR2, NOSCAN, ACOUNT(2,MAXTON,MAXIF), LSOU, ICH,
     *   NTIME, LANT, ITT(4)
      DOUBLE PRECISION TOL, TSTART, TSTOP, AVFREQ(2,MAXTON,MAXIF), TSUM,
     *   REFREQ, TT
      REAL      GMMOD, RANOD, DECNOD, IFR, MBDELY(2), DISP(2), DDISP(2),
     *   PCDELY(2,MAXIF), PCPHAS(2,MAXIF), ERDELY(2,MAXIF), XDAY,
     *   ERPHAS(2,MAXIF), TSCAN(1000), AVREAL(2,MAXTON,MAXIF),
     *   AVIMAG(2,MAXTON,MAXIF), AMP, PHLAST, PHSUM
      LOGICAL   ISAPPL, DOSCAN
      CHARACTER KEYWRD*8
      INCLUDE 'INCS:PSTD.INC'
      DATA SNLUN /23/
      DATA IFR, MBDELY, DISP, DDISP /7*0.0/
C-----------------------------------------------------------------------
C                                       Index table
      CALL GETNX (SNLUN, INDISK, CNO, CATBLK, ISUB, SNBUFF, NOSCAN,
     *   TSCAN)
      DOSCAN = (NOSCAN.GT.0) .AND. (XSOLIN.LT.0.0)
      XSOLIN = XSOLIN / (24.0 * 60.0)
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
      NPCINR = PCBUFF(5)
      NITTER = 100
      ITC  = (NUMTON  + 1) / 2
      LANT = 0
      LSOU = 0
      IR1 = 0
      IR2 = 0
      DO 500 IROW = 1,NPCINR+1
 10      PCRNO = IROW
         IF (PCRNO.LE.NPCINR) THEN
            CALL TABPC ('READ', PCBUFF, PCRNO, PCKOLS, PCNUMV, PCNPOL,
     *         TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, CABCAL,
     *         STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING PC TABLE'
               GO TO 990
               END IF
            IF (IRET.LT.0) GO TO 500
            SNNUMA = MAX (SNNUMA, ANTNUM)
            WRITE (MSGTXT,1010) IROW, ANTNUM
            IF (PRTLEV.GT.0.0) CALL MSGWRT (2)
            IF (CABCAL.EQ.DBLANK) CABCAL = 0.0D0
            IF (CABCAL.EQ.FBLANK) CABCAL = 0.0D0
            CABCAL = CABCAL * 1.0D-9
            END IF
C                                       time to average and fit??
         IF ((IR1.GT.0) .AND. ((ANTNUM.NE.LANT) .OR. (TIME.GT.TSTOP)
     *      .OR. (LSOU.NE.SOUNUM) .OR. (IROW.GT.NPCINR) .OR.
     *      (PCFREQ(1,1,1).NE.AVFREQ(1,1,1)))) THEN
C                                       average
            DO 120 IIF = 1,PCNIF
               DO 110 ICH = 1,NUMTON
                  DO 100 IPOL = 1,PCNPOL
                     IF (ACOUNT(IPOL,ICH,IIF).GT.0) THEN
                        AVREAL(IPOL,ICH,IIF) = AVREAL(IPOL,ICH,IIF) /
     *                     ACOUNT(IPOL,ICH,IIF)
                        AVIMAG(IPOL,ICH,IIF) = AVIMAG(IPOL,ICH,IIF) /
     *                     ACOUNT(IPOL,ICH,IIF)
                     ELSE
                        AVREAL(IPOL,ICH,IIF) = FBLANK
                        AVIMAG(IPOL,ICH,IIF) = FBLANK
                        END IF
 100                 CONTINUE
 110              CONTINUE
 120           CONTINUE
            IF (NTIME.GT.0) TSUM = TSUM / NTIME
C                                       get solution and residual
            CALL PCFITR (PCNPOL, PCNIF, IFSTEP, NUMTON, AVFREQ, AVREAL,
     *         AVIMAG, PRTLEV, PCDELY, PCPHAS, ERDELY, ERPHAS, WEIGHT,
     *         IRET)
            IRET = 0
C                                       write SN record
            DO 150 IPOL = 1,PCNPOL
               DO 140 IIF = 1,PCNIF
                  IF (WEIGHT(IPOL,IIF).GT.0.0) THEN
                     IF (GOTANT(IPOL,LANT,IIF).LE.0) THEN
                        DPHASE(IPOL,LANT,IIF) = PCPHAS(IPOL,IIF)
                        DDELAY(IPOL,LANT,IIF) = PCDELY(IPOL,IIF) -
     *                     CABCAL
                        GOTANT(IPOL,LANT,IIF) = 1
                        END IF
                     TOL = PCPHAS(IPOL,IIF) - DPHASE(IPOL,LANT,IIF)
                     CREAL(IPOL,IIF) = COS (TOL)
                     CIMAG(IPOL,IIF) = SIN (TOL)
                     IF (OPTYPE.EQ.'TOTA') THEN
                        CDELAY(IPOL,IIF) = PCDELY(IPOL,IIF)
                     ELSE
                        CDELAY(IPOL,IIF) = (PCDELY(IPOL,IIF) -
     *                     DDELAY(IPOL,LANT,IIF))
                        IF (DOCUT) THEN
                           IF (ABS(CDELAY(IPOL,IIF)).GT.CUTOFF) THEN
                              WRITE (MSGTXT,1080) IPOL, ANTNUM, IIF
                              CALL MSGWRT (7)
                              XDAY = TIME
                              CALL TODHMS (XDAY, ITT)
                              WRITE (MSGTXT,1081)
     *                           DDELAY(IPOL,ANTNUM,IIF)*1.E9,
     *                           PCDELY(IPOL,IIF)*1.E9, ITT
                              CALL MSGWRT (7)
                              DPHASE(IPOL,ANTNUM,IIF) = PCPHAS(IPOL,IIF)
                              DDELAY(IPOL,ANTNUM,IIF) = PCDELY(IPOL,IIF)
     *                           - CABCAL
                              CDELAY(IPOL,IIF) = (PCDELY(IPOL,IIF) -
     *                           DDELAY(IPOL,ANTNUM,IIF))
                              CREAL(IPOL,IIF) = 1.0
                              CIMAG(IPOL,IIF) = 0.0
                              END IF
                           END IF
                        END IF
                  ELSE
                     CREAL(IPOL,IIF) = FBLANK
                     CIMAG(IPOL,IIF) = FBLANK
                     CDELAY(IPOL,IIF) = FBLANK
                     END IF
 140              CONTINUE
 150           CONTINUE
C                                       write SN table record
            TIME = TSUM
            CALL TABSN ('WRIT', SNBUFF, ISNRNO, SNKOLS, SNNUMV, SNNPOL,
     *         TIME, TIMINT, LSOU, LANT, ISUB, IDFREQ, IFR, NUMNOD,
     *         MBDELY, DISP, DDISP, CREAL, CIMAG, CDELAY, CRATE, WEIGHT,
     *         REFANT, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING SN TABLE RECORD'
               GO TO 990
               END IF
C                                       correct the records
            DO 250 I = IR1,IR2
               PCRNO = I
               CALL TABPC ('READ', PCBUFF, PCRNO, PCKOLS, PCNUMV,
     *            PCNPOL, TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ,
     *            CABCAL, STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'RE-READING PC TABLE'
                  GO TO 990
                  END IF
               IF (IRET.EQ.0) THEN
                  DO 220 IIF = 1,PCNIF
                     DO 210 ICH = 1,NUMTON
                       DO 200 IPOL = 1,PCNPOL
                          REFREQ = AVFREQ(IPOL,ITC,IIF)
                          IF ((PCDELY(IPOL,IIF).NE.FBLANK) .AND.
     *                       (PCREAL(IPOL,ICH,IIF).NE.FBLANK) .AND.
     *                       (PCIMAG(IPOL,ICH,IIF).NE.FBLANK)) THEN
                              AMP = SQRT (PCREAL(IPOL,ICH,IIF)**2 +
     *                           PCIMAG(IPOL,ICH,IIF)**2)
                              PHLAST = ATAN2 (PCIMAG(IPOL,ICH,IIF),
     *                           PCREAL(IPOL,ICH,IIF))
                              PHSUM = PCPHAS(IPOL,IIF) + TWOPI *
     *                           PCDELY(IPOL,IIF) *
     *                           (PCFREQ(IPOL,ICH,IIF) - REFREQ)
                              PCREAL(IPOL,ICH,IIF) = AMP *
     *                           COS(PHLAST-PHSUM)
                              PCIMAG(IPOL,ICH,IIF) = AMP *
     *                           SIN(PHLAST-PHSUM)
                           ELSE
                              PCREAL(IPOL,ICH,IIF) = FBLANK
                              PCIMAG(IPOL,ICH,IIF) = FBLANK
                              END IF
 200                       CONTINUE
 210                    CONTINUE
 220                 CONTINUE
                  PCRNO = I
                  CALL TABPC ('WRIT', PCBUFO, PCRNO, PCKOLS, PCNUMV,
     *               PCNPOL,TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ,
     *               CABCAL,STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT PC TABLE'
                     GO TO 990
                     END IF
                  END IF
 250           CONTINUE
            IR1 = 0
            LANT = 0
            LSOU = 0
            IF (IROW.LE.NPCINR) GO TO 10
C                                       add in to sums
         ELSE IF (IR1.GT.0) THEN
            IR2 = IROW
            DO 320 IIF = 1,PCNIF
               DO 310 ICH = 1,NUMTON
                  DO 300 IPOL = 1,PCNPOL
                     IF ((PCREAL(IPOL,ICH,IIF).NE.FBLANK) .AND.
     *                  (PCIMAG(IPOL,ICH,IIF).NE.FBLANK)) THEN
                        ACOUNT(IPOL,ICH,IIF) = ACOUNT(IPOL,ICH,IIF) + 1
                        AVREAL(IPOL,ICH,IIF) = AVREAL(IPOL,ICH,IIF) +
     *                     PCREAL(IPOL,ICH,IIF)
                        AVIMAG(IPOL,ICH,IIF) = AVIMAG(IPOL,ICH,IIF) +
     *                     PCIMAG(IPOL,ICH,IIF)
                        END IF
 300                 CONTINUE
 310              CONTINUE
 320           CONTINUE
            TSUM = TSUM + TIME
            NTIME = NTIME + 1
C                                       start new sum
         ELSE
            IR1 = IROW
            IR2 = IROW
            DO 420 IIF = 1,PCNIF
               DO 410 ICH = 1,NUMTON
                  DO 400 IPOL = 1,PCNPOL
                     IF ((PCREAL(IPOL,ICH,IIF).NE.FBLANK) .AND.
     *                  (PCIMAG(IPOL,ICH,IIF).NE.FBLANK)) THEN
                        ACOUNT(IPOL,ICH,IIF) = 1
                        AVREAL(IPOL,ICH,IIF) = PCREAL(IPOL,ICH,IIF)
                        AVIMAG(IPOL,ICH,IIF) = PCIMAG(IPOL,ICH,IIF)
                     ELSE
                        ACOUNT(IPOL,ICH,IIF) = 0
                        AVREAL(IPOL,ICH,IIF) = 0.0
                        AVIMAG(IPOL,ICH,IIF) = 0.0
                        END IF
                     AVFREQ(IPOL,ICH,IIF) = PCFREQ(IPOL,ICH,IIF)
 400                 CONTINUE
 410              CONTINUE
 420           CONTINUE
            LANT = ANTNUM
            LSOU = SOUNUM
C                                       which scan
            NTIME = 1
            TSUM = TIME
            TSTART = TIME
            IF (.NOT.DOSCAN) TSTOP = TSTART + XSOLIN
            IF (NOSCAN.LE.0) THEN
               IF (DOSCAN) TSTOP = 10000.
            ELSE IF (TIME.LT.TSCAN(1)) THEN
               IF (DOSCAN) TSTOP = TSCAN(1) - 0.1/(24.*3600.)
            ELSE IF (TIME.GT.TSCAN(NOSCAN)) THEN
               IF (DOSCAN) TSTOP = 10000.
            ELSE
               DO 430 I = 1,NOSCAN-1
                  IF ((TIME.GE.TSCAN(I)) .AND. (TIME.LE.TSCAN(I+1)))
     *               THEN
                     IF (DOSCAN) THEN
                        TSTOP = TSCAN(I+1) - 0.1 / (24.*3600.)
                     ELSE
                        TT = TSCAN(I+1)-0.1/(24.*3600.)
                        TSTOP = MIN (TSTOP, TT)
                        END IF
                     END IF
 430             CONTINUE
               END IF
            END IF
 500     CONTINUE
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
 1080 FORMAT ('Polarization',I2,' antenna',I4,' IF',I3,
     *   ' zero pt delay changed')
 1081 FORMAT (5X,'from',F9.2,' to',F9.2,' at',I3,'/',2(I2.2,':'),I2.2)
      END
