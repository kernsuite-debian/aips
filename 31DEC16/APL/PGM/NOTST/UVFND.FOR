LOCAL INCLUDE 'UVFND.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAME*12, CLASS*6, STOKX*4, XSOUR(30)*16,
     *   OPCODE*4, LPNAME*48, TITL1*132, TITL2*132, SCRTCH*132, LINE*132
      HOLLERITH XNAME(3), XCLASS(2), XSTOKE, XXSOUR(4,30), XCALC,
     *   XOPCOD, XLPNAM(12)
      REAL      XSEQ, XDISK, XBCHAN, XECHAN, XCHAN, XNCHAV, XBIF, XNIT,
     *   UVRANG(2), XQUAL, XTIME(8), XBAND, XFREQ, XFQID, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   APARM(10), DOSCAL, XWTUV, DOCRT, XANT(50), XBAS(50)
      REAL      BUFF(256), RADIUS(2), POSANG(2), FMAX(4), XAMP, XWT,
     *   NWT, UVM, RPARM(50), VIS(3,MAXCIF), XSUBA
      LOGICAL   DOMAX, DOUVBX, DOVCLP, DOUVPI, ISUVR, MULTI, DOWTUV,
     *   DESEL, DOBLIN
      INTEGER   LUN, FIND, PLUN, PIND, CNO, NCH, SEQ, DISK, USERID,
     *   NITER, IBIF, CATOLD(256), NACROS, PAGE, IPCNT, IANT(50),
     *   IBAS(50), NBASS, NANTS, NCHAV, INSNUM
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XBCHAN, XECHAN,
     *   XNCHAV, XCHAN, XBIF, XNIT, UVRANG, XSTOKE, XXSOUR, XQUAL,
     *   XCALC, XTIME, XBAND, XFREQ, XFQID, XANT, XBAS, XSUBA, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH,
     *   XOPCOD, APARM, DOSCAL, XWTUV, DOCRT, XLPNAM
      COMMON /CHPARM/ NAME, CLASS, STOKX, XSOUR, OPCODE, LPNAME,
     *   TITL1, TITL2, LINE, SCRTCH
      COMMON /FNDUVC/ BUFF, RADIUS, POSANG, FMAX, NITER, DOMAX, DOUVBX,
     *   DOVCLP, DOUVPI, ISUVR, MULTI, LUN, FIND, NCH, PLUN, PIND, CNO,
     *   SEQ, DISK, USERID, IBIF, CATOLD, NACROS, PAGE, IPCNT, DOWTUV,
     *   XAMP, XWT, NWT, UVM, IANT, IBAS, NBASS, DESEL, NANTS, DOBLIN,
     *   NCHAV, RPARM, VIS, INSNUM
LOCAL END
      PROGRAM UVFND
C-----------------------------------------------------------------------
C! UVFND locates questionable data in UV file and prints them.
C# UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2002-2004, 2006-2007, 2009-2011
C;  Copyright (C) 2013-2015
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
C   UVFND is an AIPS system task to locate questionable data in a
C   UV data base and to print them.
C   Inputs:
C     INNAME    R(3)      File name: 12 chars
C     INCLASS   R(2)      File class: 6 chars
C     INSEQ     R         File seq #
C     INDISK    R         Disk volume on which file resides
C     BCHAN     R         Spectral channel number
C     ECHAN     R         Spectral channel number
C     CHANNEL   R         Spectral channel number
C     BIF       R         IF number to test
C     NITER     R         Limit on number of lines to print
C     UVRANGE   R(2)      Range of wavelengths to check in 1000's
C     STOKES    R         Limit to data satisfying mapping requirements
C                         of STOKES = 'I','IV','IQU','IQUV',etc.
C     SOURCES   H(4,30)   Source list
C     TIMERANG  R(8)      Timerange
C     SELBAND   R         Bandwidth
C     SELFREQ   R         Frequency
C     FREQID    R         FQ id.
C     SUBARRAY  R         Subarray
C     DOCALIB   R         >0 => calibrate
C     GAINUSE   R         CL/SN table to use
C     FLAGVER   R         FG table to apply
C     OPCODE    R         Opcode: 4 chars
C                            'CLIP' : print high fluxes
C                            'VCLP' : print high (RR - LL)/2 points
C                            'FRNG' : print points giving spec.
C                                     fringe freq and pa
C     APARM     R(10)     for CLIP ; (1) max cutoff IPOL (Jy)
C                                    (2) max cutoff cross-pol (Jy)
C                                    (3) min cutoff IPOL (Jy)
C                                    (4) min cutoff cross-pol (Jy)
C                                    (6) - (8) for phase (deg)
C                         for VCLP : (1) cutoff VPOL (Jy)
C                         for FRNG : (1) fringe spacing arc sec
C                                    (2) range of fringe spacing
C                                    (3) position angle (CCW from N)
C                                    (4) range in position angle
C     DOCRT     R         > 0 -> use CRT, else line printer
C     OUTPRINT  H         File name to keep printer output
C-----------------------------------------------------------------------
      LOGICAL   RQUICK
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'UVFND.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       Init, open file
      CALL FNDUIN (RQUICK, IRET)
C                                       Scaling
      IF (IRET.EQ.0) CALL FNDUSC (IRET)
C                                       Do it
      IF (IRET.EQ.0) THEN
C                                       Interferometer data
         IF (TYPUVD.LE.0) THEN
            CALL FNDUPR (IRET)
C                                       Single dish data
         ELSE
            CALL FNDSPR (BCHAN, ECHAN, IRET)
            END IF
         END IF
C                                       Close up shop
      CALL DIETSK (IRET, RQUICK, BUFF)
C
 999  STOP
      END
      SUBROUTINE FNDUIN (RQUICK, IRET)
C-----------------------------------------------------------------------
C   FNDUIN inits the task UVFND and opens the UV file and printer.
C   Outputs:
C      RQUICK   L   T => AIPS has been restarted
C      IRET     I   error code: 0 => keep going, else quit
C-----------------------------------------------------------------------
      LOGICAL   RQUICK
      INTEGER   IRET
C
      CHARACTER PRGM*6, OPS(5)*4, UTYPE*2, SCALED*10
      LOGICAL   F, MATCH
      INTEGER   NPARM, I, IERR, IROUND
      REAL      RC, R
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'UVFND.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA OPS /'CLIP','FRNG','VCLP','UVBX','WTUV'/
      DATA F /.FALSE./
      DATA PRGM /'UVFND '/
C-----------------------------------------------------------------------
C                                       Init I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Init DSEL.INC
      CALL SELINI
C                                       Get input parameters.
      NPARM = 287
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFF, IRET)
      IF (IRET.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IF (IRET.EQ.1) GO TO 999
C                                       Restart AIPS.
 10   IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      RQUICK = (RQUICK) .AND. (DOCRT.LE.0.0)
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (4, 1, XSTOKE, STOKX)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      DO 15 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 15      CONTINUE
C                                       Decode input
      IF (DOSCAL.GT.0.0) THEN
         SCALED = ' Scaled Jy'
      ELSE
         SCALED = ' Jy'
         END IF
      USERID = NLUSER
      SEQ = XSEQ + 0.1
      DISK = XDISK + 0.1
      SELQUA = IROUND (XQUAL)
      BIF = XBIF + 0.1
      IF (BIF.LE.0) BIF = 1
      NITER = XNIT + 0.1
      IF (NITER.LE.0) NITER = 10 * (PRTMAX - 7)
      IF (UVRANG(1).LE.0.0) UVRANG(1) = 0.0
      IF (UVRANG(2).LE.UVRANG(1)) UVRANG(2) = 1.E9
      ISUVR = (UVRANG(1).GT.0.001) .OR. (UVRANG(2).LT.9.8E8)
      DOMAX  = OPCODE.EQ.OPS(1)
      DOUVPI = OPCODE.EQ.OPS(2)
      DOVCLP = OPCODE.EQ.OPS(3)
      DOUVBX = OPCODE.EQ.OPS(4)
      DOWTUV = OPCODE.EQ.OPS(5)
      IF (DOWTUV) DOMAX = .TRUE.
      IF (.NOT.DOWTUV) XWTUV = -1.0
C                                       Crunch Antenna adverbs
      CALL SETANT (50, XANT, XBAS, NANTS, NBASS, IANT, IBAS, DESEL)
      DOBLIN = (NANTS.GT.0) .OR. (NBASS.GT.0)
C                                       Known OPCODE?
      IF ((.NOT.DOMAX) .AND. (.NOT.DOUVPI) .AND. (.NOT.DOVCLP) .AND.
     *   (.NOT.DOUVBX)) THEN
         WRITE (MSGTXT,1010) OPCODE
         CALL MSGWRT (6)
         END IF
C                                       Open file and get CATBLK.
      LUN = 16
      UTYPE = 'UV'
      CALL MAPOPN ('READ', DISK, NAME, CLASS, SEQ, UTYPE, USERID, LUN,
     *   FIND, CNO, CATBLK, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Save input file CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Get info from CATBLK.
      CALL UVPGET (IERR)
      IERR = 0
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAME
      UCLAS = CLASS
      UDISK = DISK
      IUDISK = DISK
      IUCNO = CNO
      USEQ = SEQ
C                                       Check IF
      IF ((JLOCIF.LE.0) .OR. (CATBLK(KINAX+JLOCIF).LE.1)) BIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF))) THEN
         WRITE (MSGTXT,1030) BIF, CATBLK(KINAX+JLOCIF)
         CALL MSGWRT (8)
         GO TO 500
         END IF
C                                       Check spectral channel.
      BCHAN = XBCHAN + 0.1
      ECHAN = XECHAN + 0.1
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MIN (ECHAN, CATBLK(KINAX+JLOCF))
      NCHAV = XNCHAV + 0.1
      NCHAV = MAX (1, MIN (ECHAN-BCHAN+1, NCHAV))
      NCH = XCHAN + 0.1
      IF (NCH.LE.0) NCH = (BCHAN + ECHAN + 1) / 2
      NCH = ((NCH - BCHAN) / NCHAV) * NCHAV + BCHAN
      IF (NCH.GT.CATBLK(KINAX+JLOCF)) THEN
         WRITE (MSGTXT,1032) NCH, CATBLK(KINAX+JLOCF)
         CALL MSGWRT (8)
         GO TO 500
         END IF
C                                       Open printer
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, PLUN, PIND, NACROS, BUFF, IERR)
      IF (IERR.NE.0) GO TO 500
      IRET = 0
      PAGE = 0
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
C                                       function parameters
      IF (DOMAX) THEN
         IF (TYPUVD.LE.0) THEN
            IF ((APARM(2).LE.0.0) .AND. (APARM(1).LE.0.0))
     *         APARM(1) = 1.0E8
            IF ((APARM(2).LE.0.0) .AND. (APARM(1).GT.0.0))
     *         APARM(2) = 1.E8
            IF ((APARM(1).LE.0.0) .AND. (APARM(2).GT.0.0))
     *         APARM(1) = 1.E8
            IF (APARM(3).LE.0.0) APARM(3) = -1.0E8
            IF (APARM(4).LE.0.0) APARM(4) = -1.0E8
            IF (APARM(5).EQ.0.0) APARM(5) = 200.0
            IF (APARM(6).EQ.0.0) APARM(6) = 200.0
            IF (APARM(7).EQ.0.0) APARM(7) = -200.0
            IF (APARM(8).EQ.0.0) APARM(8) = -200.0
            IF (APARM(7).GE.APARM(5)) THEN
               APARM(5) = 200.0
               APARM(7) = -200.0
               END IF
            IF (APARM(8).GE.APARM(6)) THEN
               APARM(6) = 200.0
               APARM(8) = -200.0
               END IF
            END IF
C                                       Line printer info
         IF ((APARM(1).LT.1.E5) .AND. (DOCRT.GT.-2.5)) THEN
            WRITE (LINE,1035) '>', APARM(1), SCALED
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((APARM(2).LT.1.E5) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1036) '>', APARM(2), SCALED
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((APARM(3).GT.-1.E5) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1035) '<', APARM(3), SCALED
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((APARM(4).GT.-1.E5) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1036) '<', APARM(4), SCALED
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((APARM(5).LT.180.0) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1037) '>', APARM(5)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((APARM(6).LT.180.0) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1037) '>', APARM(6)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((APARM(7).GT.-180.0) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1037) '<', APARM(7)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((APARM(8).GT.-180.0) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1037) '<', APARM(8)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF ((ISUVR) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1039) UVRANG
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF (TYPUVD.LE.0)  THEN
            APARM(1) = APARM(1) * APARM(1)
            APARM(2) = APARM(2) * APARM(2)
            IF (APARM(3).GT.0.0) APARM(3) = APARM(3) * APARM(3)
            IF (APARM(4).GT.0.0) APARM(4) = APARM(4) * APARM(4)
            APARM(5) = APARM(5) * DG2RAD
            APARM(6) = APARM(6) * DG2RAD
            APARM(7) = APARM(7) * DG2RAD
            APARM(8) = APARM(8) * DG2RAD
            END IF
C                                       UV anulus
      ELSE IF (DOUVPI) THEN
         RC = 3600. * 180. / 3.14159 / 1000.0
         R = ABS (APARM(1) + APARM(2))
         IF (R.EQ.0.0) R = 1.0
         RADIUS(1) = RC / R
         R = ABS (APARM(1) - APARM(2))
         IF (R.EQ.0.0) R = 1.0
         RADIUS(2) = RC / R
         POSANG(1) = APARM(3) - ABS(APARM(4))
         POSANG(2) = APARM(3) + ABS(APARM(4))
         IF (POSANG(1).LT.-90.0) THEN
            POSANG(1) = POSANG(1) + 180.0
            POSANG(2) = POSANG(2) + 180.0
            END IF
         IF (ISUVR) THEN
            IF ((UVRANG(1).GT.RADIUS(1)) .OR. (UVRANG(2).LT.RADIUS(2)))
     *         THEN
               IF ((UVRANG(1).LT.RADIUS(2)) .AND.
     *            (UVRANG(2).GT.RADIUS(1))) THEN
                  RADIUS(1) = MAX (RADIUS(1), UVRANG(1))
                  RADIUS(2) = MIN (RADIUS(2), UVRANG(2))
                  WRITE (MSGTXT,1043)
                  CALL MSGWRT (3)
               ELSE
                  WRITE (MSGTXT,1044)
                  CALL MSGWRT (3)
                  END IF
               UVRANG(1) = 0.
               UVRANG(2) = 1.E9
               ISUVR = .FALSE.
               END IF
            END IF
         IF (DOCRT.GT.-2.5) THEN
            WRITE (LINE,1046) RADIUS, POSANG
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         RADIUS(1) = RADIUS(1) * RADIUS(1)
         RADIUS(2) = RADIUS(2) * RADIUS(2)
C                                       V polariz check
      ELSE IF (DOVCLP) THEN
         STOKX = 'V'
         IF (APARM(1).LE.0.0) APARM(1) = 1.0
         IF (DOCRT.GT.-2.5) THEN
            WRITE (LINE,1050) APARM(1)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         APARM(1) = APARM(1) * APARM(1)
         IF ((ISUVR) .AND. (DOCRT.GT.-2.5)) THEN
            IF (XNIT.LE.0.9) NITER = NITER - 1
            WRITE (LINE,1039) UVRANG
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         CALL RFILL (9, 0.0, APARM(2))
C                                       Box in UV plane
      ELSE IF (DOUVBX) THEN
         R = ABS (APARM(2))
         IF (R.LE.0.0) R = 1.0
         RADIUS(1) = APARM(1) - R
         RADIUS(2) = APARM(1) + R
         R = ABS(APARM(4))
         IF (R.LE.0.) R = 1.0
         POSANG(1) = APARM(3) - R
         POSANG(2) = APARM(3) + R
         UVRANG(1) = 0.
         UVRANG(2) = 1.E9
         ISUVR = .FALSE.
         IF ((TYPUVD.LE.0) .AND. (DOCRT.GT.-2.5)) THEN
            WRITE (LINE,1065) RADIUS, POSANG
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
         ELSE IF ((TYPUVD.GT.0) .AND. (DOCRT.GT.-2.5)) THEN
            WRITE (LINE,1066) RADIUS, POSANG
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
      IF ((IPCNT.LT.100) .AND. (DOCRT.GT.-2.5)) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Scale UVRANG
      UVRANG(1) = UVRANG(1) * UVRANG(1)
      UVRANG(2) = UVRANG(2) * UVRANG(2)
C                                       Close map on error
 500  CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1500) IERR
         CALL MSGWRT (6)
         END IF
C                                       Setup for UVGET.
C                                       See if a multiple source file
      CALL MULSDB (CATBLK, MULTI)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAME
      UCLAS = CLASS
      UDISK = DISK
      USEQ = SEQ
C                                       Stokes' parameter
      IF (STOKX.EQ.'CORR ') THEN
         STOKES = ' '
      ELSE IF (STOKX.EQ.' ') THEN
         STOKES = 'I'
      ELSE
         STOKES = STOKX
         END IF
      DO 510 I = 1,30
         SOURCS(I) = XSOUR(I)
 510     CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      DXTIME = 0.0
      EIF = BIF
      IBIF = MAX (1, BIF)
C                                       FG table version
      FGVER = IROUND (XFLAG)
C                                       Calibration
      SUBARR = IROUND (XSUBA)
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      CLUSE = IROUND (XGUSE)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      BLVER = IROUND (XBLVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISK, CNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1510)
         CALL MSGWRT (9)
         IRET = 1
         END IF
      GO TO 999
C                                       printer error
 900  IRET = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ONLY INTEGRITY CHECK ON UNKNOWN OPCODE =''',A4,'''')
 1025 FORMAT ('ERROR:',I7,' OPENING UV FILE')
 1030 FORMAT ('IF',I5,' .GT. MAX =',I5)
 1032 FORMAT ('CHANNEL',I5,' .GT. MAX =',I5)
 1035 FORMAT ('Will print all total int. points   ',A1,F9.3,A)
 1036 FORMAT ('Will print all polarization points ',A1,F9.3,A)
 1037 FORMAT ('Will print all total int. points   ',A1,F9.3,' degrees')
 1039 FORMAT ('Limited to baselines between',2(1PE13.4),' klambda')
 1043 FORMAT ('WARNING: REQUESTED RADII LIMITED BY UVRANGE')
 1044 FORMAT ('WARNING: REQUESTED RADII NOT IN UVRANGE.  UVRANGE ',
     *   'IGNORED')
 1046 FORMAT ('Will print points between radii',2F9.1,' klambda, PA',
     *   2F5.0)
 1050 FORMAT ('Will print all V pol. points >',F8.4,' Jy')
 1065 FORMAT ('Will print points between U',2F9.1,' klambda, V',
     *   2F9.1,' klambda')
 1066 FORMAT ('Will print points between RA',2F9.1,' deg, DEC',
     *   2F9.1,' deg')
 1500 FORMAT ('ERROR:',I7,' CLOSING UV FILE')
 1510 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE FNDUSC (IRET)
C-----------------------------------------------------------------------
C   FNDUSC reads the beginning of the UV file and finds estimates of the
C   u-v-w, flux, and weight extrema.
C   Output:
C      IRET   I     Return code: 0 success or user requests end,
C                      else bad
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, L, INDEX, IROUND, IA1, IA2, ICH1, ICH2, NPOL, LIF,
     *   IERR, II, JJ, KK, KKK, IPOINT, NNCH, NUMCH, K, NUMCHM, OTYPE,
     *   COUNT, IAPCNT, CINC, LCH, LL, LSOU, ISOU, NXLUN
      LOGICAL   REQBAS
      REAL      AMP, TEMP, UVN, TPHS, U, V, W, XX, YY, YAMP, UVSCAL,
     *   CATR(256), XA(4), YA(4), WA(4), SPIX(3)
      DOUBLE PRECISION CATD(128)
      HOLLERITH CATH(256)
      INCLUDE 'UVFND.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATH, CATD, CATR)
C-----------------------------------------------------------------------
      SPIX(1) = 0.0
      SPIX(3) = 0.0
      SPIX(2) = -1.E6
      INSNUM = 0
      LSOU = -1
      NXLUN = 100
C                                       Open UV data file.
      CALL UVGET ('INIT', RPARM, VIS, IERR)
      IF (IERR.NE.0) GO TO 999
      LCH = NCH - BCHAN + 1
      NPOL = CATBLK(KINAX+JLOCS)
      IF ((NSOUWD.EQ.1) .AND. (DOSWNT)) INSNUM = SOUWAN(1)
C                                       Setup
      IRET = 8
      CINC = 2
C                                       Multiple sources?
      MULTI = MULTI .AND. (ILOCSU.GE.0)
      UVM = 0.0
      XAMP = 0.0
      XWT = 0.0
      NWT = 1.E10
C                                        Determine no. channels to do
      FREQ = FREQ - (CATR(KRCRP+JLOCF)-LCH) * CATR(KRCIC+JLOCF)
      UVSCAL = FREQ / UVFREQ
      NUMCHM = CATBLK(KINAX+JLOCF) - LCH + 1
      NUMCH = (NACROS - 52) / (15 * NCOR)
      NUMCH = MIN (NUMCH, NUMCHM)
      OTYPE = 1
      IF ((NUMCHM.GT.1) .AND. (NACROS.LE.80) .AND. (NCOR.EQ.1)) OTYPE =
     *   2
      IF ((OTYPE.EQ.2) .AND. (NACROS.LE.72)) OTYPE = 3
      IF (OTYPE.GT.1) NUMCH = 2
      IF (NUMCH.LT.1) THEN
         NUMCH = 1
         END IF
      ICH1 = NCH
      ICH2 = NCH + (NUMCH - 1) * NCHAV
      IF (ICH2.GT.ECHAN) THEN
         IF ((XCHAN.LE.0.4) .AND. (NCH-NCHAV.GE.BCHAN)) THEN
             NCH = NCH-NCHAV
             ICH2 = NCH + (NUMCH - 1) * NCHAV
             IF (ICH2.GT.ECHAN) THEN
                IF (NCH-NCHAV.GE.BCHAN) THEN
                   NCH = NCH-NCHAV
                   ICH2 = NCH + (NUMCH - 1) * NCHAV
                   END IF
                END IF
             END IF
         NUMCH = (ECHAN - NCH) / NCHAV + 1
         ICH2 = NCH + (NUMCH - 1) * NCHAV
         END IF
      NNCH = NCH - (NCHAV+1) / 2
      DO 15 II = 1,4,NCOR
         NNCH = NNCH + NCHAV
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
 10         CONTINUE
 15      CONTINUE
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
      COUNT = 0
      IAPCNT = 0
      IPOINT = 1
C                                       Read buffer.
 100  CONTINUE
         COUNT = COUNT + 1
         CALL UVGET ('READ', RPARM, VIS, IERR)
C                                       Done?
         IF (IERR.EQ.-1) THEN
            IERR = 0
            GO TO 900
            END IF
         IF (IERR.NE.0) GO TO 999
C                                       Update counters
C                                       Determine antennas.
         IF (ILOCB.GE.0) THEN
            IA1 = RPARM(1+ILOCB)/256. + 0.1
            IA2 = RPARM(1+ILOCB) - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
C                                       If selecting by baseline
         IF (DOBLIN) THEN
            IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT, NANTS, IBAS, NBASS))
     *         GO TO 100
            END IF
C                                       scaling info
         IF (DOSCAL.GT.0.0) THEN
            ISOU = 0
            IF (ILOCSU.GE.0) ISOU = IROUND (RPARM(1+ILOCSU))
            IF (ISOU.LE.0) ISOU = INSNUM
            IF (ISOU.NE.LSOU) THEN
               LSOU = ISOU
               CALL GETSOU (LSOU, DISK, CNO, CATUV, NXLUN, IRET)
               DO 105 LIF = BIF,EIF
                  IF (FLUX(1,LIF).LE.1.E-10) FLUX(1,LIF) = 1.0
 105              CONTINUE
               IF (DOSCAL.GT.1.5) THEN
                  K = DOSCAL - 0.5
                  CALL FNDSPX (DISK, CNO, LSOU, FRQSEL, CATUV, K, SPIX,
     *               IRET)
               ELSE
                  SPIX(2) = 0.0
                  END IF
               END IF
            CALL UVFNDS (FLUX, SPIX, NPOL, VIS)
            END IF
C                                       Convert uvw to kilo lamda.
         U = RPARM(1+ILOCU) * 0.001 * UVSCAL
         V = RPARM(1+ILOCV) * 0.001 * UVSCAL
         W = RPARM(1+ILOCW) * 0.001 * UVSCAL
         UVM = MAX (UVM, U)
         UVM = MAX (UVM, V)
         UVM = MAX (UVM, W)
         UVN = U
         UVN = MIN (UVN, V)
         UVN = MIN (UVN, W)
         UVM = MAX (UVM, -10.0*UVN)
C                                       Use this point
         K = 0
         YAMP = 0.0
         DO 145 KK = BCHAN,ECHAN,NCHAV
            CALL RFILL (4, 0.0, XA)
            CALL RFILL (4, 0.0, YA)
            CALL RFILL (4, 0.0, WA)
            DO 120 LL = KK,MIN(ECHAN,KK+NCHAV-1)
               DO 110 KKK = 1,NCOR
                  K = K + 1
                  INDEX = ((LL-BCHAN)*INCF + (KKK-1)*INCS) / 3 + 1
                  TPHS = ABS (VIS(3,INDEX))
                  XWT = MAX (XWT,  TPHS)
                  IF (TPHS.GT.0.0) NWT = MIN (NWT, TPHS)
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     XA(KKK) = XA(KKK) + VIS(1,INDEX)*VIS(3,INDEX)
                     YA(KKK) = YA(KKK) + VIS(2,INDEX)*VIS(3,INDEX)
                     WA(KKK) = WA(KKK) + VIS(3,INDEX)
                     END IF
 110              CONTINUE
 120           CONTINUE
            DO 140 KKK = 1,NCOR
               IF (WA(KKK).GT.0.0) THEN
                  XX = XA(KKK) / WA(KKK)
                  YY = YA(KKK) / WA(KKK)
               ELSE
                  XX = 0.0
                  YY = 0.0
                  END IF
               IF (IA1.NE.IA2) THEN
                  AMP = SQRT (XX*XX+YY*YY)
               ELSE
                  AMP = XX
                  END IF
               XAMP = MAX (XAMP, AMP)
               YAMP = MIN (YAMP, AMP)
 140           CONTINUE
 145        CONTINUE
         XAMP = MAX (XAMP, -10.0*YAMP)
         IF (COUNT.LT.MIN(15000,NVIS)) GO TO 100
C                                       Close files.
 900  CALL UVGET ('CLOS', RPARM, VIS, IERR)
      IF (IERR.NE.0) GO TO 999
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE FNDUPR (IRET)
C-----------------------------------------------------------------------
C   FNDUPR reads the UV file and prints those samples meeting the
C   criteria given by OPCODE and APARM.
C   Output:
C      IRET   I     Return code: 0 success or user requests end,
C                      else bad
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER  CH1*1, CH2*1, JSTOKE(4,2)*2, ISTOKE(8)*2, TCHAR*12,
     *   LLCH*4, MMCH*4
      LOGICAL   DOIT, REQBAS
      INTEGER   ICH(8), HM(2), DD(2), I, L, ITT(4), IWT(6), PHASE(6),
     *   INDEX, IROUND, IA1, IA2, ICH1, ICH2, IERR, II, JJ, KK, KKK, LL,
     *   IPOINT, JCOR, LIMIT2, NNCH, NUMCH, K, NUMCHM, OTYPE, XCOUNT,
     *   COUNT, IAPCNT, NOK, SOUID, OLDSID, CINC, LCH, ISOU, LSOU, NPOL,
     *   LIF, NXLUN
      REAL      RSEC, DSEC, AMP(6), TEMP, UVN, TPHS, P, R, RADEG, U, V,
     *   W, XX, YY, YAMP, XDAY, WTSC, UVSCAL, CATR(256), XA(4), YA(4),
     *   WA(4), SPIX(3)
      DOUBLE PRECISION CATD(128)
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'UVFND.INC'
      EQUIVALENCE (CATBLK, CATH, CATD, CATR)
      DATA JSTOKE /'RR','LL','RL','LR', 'I ','Q ','U ','V '/
      DATA TCHAR /' Amp Phas Wt'/
C-----------------------------------------------------------------------
      IRET = 8
      LSOU = -1
      SPIX(1) = 0.0
      SPIX(2) = -1.E6
      SPIX(3) = 0.0
      NXLUN = 100
C                                       Open UV data file.
      CALL UVGET ('INIT', RPARM, VIS, IERR)
      IF (IERR.NE.0) GO TO 999
      LCH = NCH - BCHAN + 1
      NPOL = CATBLK(KINAX+JLOCS)
C                                       Setup
      RADEG = 180.0 / 3.14159
      CINC = 2
C                                       range parameters
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      TEMP = NWT * WTSC
      IF (TEMP.LT.1.0) THEN
         MSGTXT = 'Full dynamic range of weights cannot be printed'
         CALL MSGWRT (6)
         END IF
C                                       Multiple sources?
      MULTI = MULTI .AND. (ILOCSU.GE.0)
      SOUID = -1
      OLDSID = -1
C                                        Determine no. channels to do
      FREQ = FREQ - (CATR(KRCRP+JLOCF)-LCH) * CATR(KRCIC+JLOCF)
      UVSCAL = FREQ / UVFREQ
      NUMCHM = CATBLK(KINAX+JLOCF) - LCH + 1
      NUMCH = (NACROS - 52) / (15 * NCOR)
      NUMCH = MIN (NUMCH, NUMCHM)
      OTYPE = 1
      IF ((NUMCHM.GT.1) .AND. (NACROS.LE.80) .AND. (NCOR.EQ.1)) OTYPE =
     *   2
      IF ((OTYPE.EQ.2) .AND. (NACROS.LE.72)) OTYPE = 3
      IF (OTYPE.GT.1) NUMCH = 2
      IF (NUMCH.LT.1) THEN
         NUMCH = 1
         IF (NACROS.LE.80) OTYPE = 2
         IF (NACROS.LE.72) OTYPE = 3
         IF (NCOR.NE.2) THEN
            OTYPE = 4
            IF (NACROS.LE.80) OTYPE = 5
            IF (NACROS.LE.72) OTYPE = 6
            END IF
         END IF
      ICH1 = NCH
      ICH2 = NCH + (NUMCH - 1) * NCHAV
      IF (ICH2.GT.ECHAN) THEN
         IF ((XCHAN.LE.0.4) .AND. (NCH-NCHAV.GE.BCHAN)) THEN
             NCH = NCH-NCHAV
             ICH2 = NCH + (NUMCH - 1) * NCHAV
             IF (ICH2.GT.ECHAN) THEN
                IF (NCH-NCHAV.GE.BCHAN) THEN
                   NCH = NCH-NCHAV
                   ICH2 = NCH + (NUMCH - 1) * NCHAV
                   END IF
                END IF
             END IF
         NUMCH = (ECHAN - NCH) / NCHAV + 1
         ICH2 = NCH + (NUMCH - 1) * NCHAV
         END IF
      LIMIT2 = NCOR * NUMCH
      NNCH = NCH - (NCHAV+1) / 2
      DO 15 II = 1,LIMIT2,NCOR
         NNCH = NNCH + NCHAV
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
            ISTOKE(INDEX) = JSTOKE(I,L)
 10         CONTINUE
 15      CONTINUE
C                                       Convert coordinates.
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*CINC), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*CINC), MMCH)
      IF (LLCH(:2).EQ.'RA') THEN
         CALL COORDD (1, RA, CH1, HM, RSEC)
      ELSE
         CALL COORDD (2, RA, CH1, HM, RSEC)
         END IF
      CALL COORDD (2, DEC, CH2, DD, DSEC)
      FREQ = FREQ * 1.0E-9
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write first page header
      IF (DOCRT.GT.-2.5) THEN
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) NAME, CLASS, SEQ, DISK, USERID, BCHAN,
     *         ECHAN, NCHAV, IBIF, SUBARR
         ELSE
            WRITE (LINE,1021) NAME, CLASS, SEQ, DISK, USERID, BCHAN,
     *         ECHAN, NCHAV, IBIF, SUBARR
            END IF
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1022) SOURCE, LLCH, CH1, HM, RSEC, MMCH, CH2, DD,
     *      DSEC
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1023) FREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1024) WTSC
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
C                                       Print cal and flag info
         IF (XDOCAL.GT.0.0) THEN
            LINE = 'Applying calibration'
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF (XFLAG.GT.0.0) THEN
            LINE = 'Applying flagging table'
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
C                                       Page titles
      IF (OTYPE.EQ.1) THEN
         WRITE (TITL1,1040) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         WRITE (TITL2,1050) (TCHAR, JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.2) THEN
         WRITE (TITL1,1041) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         WRITE (TITL2,1051) (TCHAR, JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.3) THEN
         WRITE (TITL1,1042) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         WRITE (TITL2,1052) (TCHAR, JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.4) THEN
         WRITE (TITL1,1043) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         WRITE (TITL2,1053) (TCHAR, JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.5) THEN
         WRITE (TITL1,1044) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         WRITE (TITL2,1054) (TCHAR, JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.6) THEN
         WRITE (TITL1,1045) SOURCE, ISORT, (ICH(JCOR), ISTOKE(JCOR),
     *      JCOR = 1,LIMIT2)
         WRITE (TITL2,1055) (TCHAR, JCOR = 1,LIMIT2)
         END IF
C                                       Titles for single source
      IF (.NOT.MULTI) THEN
         LINE = ' '
         IF (DOCRT.GT.-2.5) CALL PRTLIN (PLUN, PIND, DOCRT, NACROS,
     *      TITL1, TITL2, LINE,IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      COUNT = 0
      XCOUNT = 0
      IAPCNT = 0
      IPOINT = 1
C                                       Read buffer.
 100  CONTINUE
         COUNT = COUNT + 1
         CALL UVGET ('READ', RPARM, VIS, IERR)
C                                       Done?
         IF (IERR.EQ.-1) THEN
            IERR = 0
            GO TO 200
            END IF
         IF (IERR.NE.0) GO TO 999
C                                       Update counters
         XCOUNT = XCOUNT + 1
         DOIT = .FALSE.
C                                       Decode time.
         XDAY = RPARM(1+ILOCT)
         IF ((XDAY.LT.0.0) .OR. (XDAY.GT.1000.)) DOIT = .TRUE.
         CALL TODHMS (XDAY, ITT)
C                                       Determine antennas.
         IF (ILOCB.GE.0) THEN
            IA1 = RPARM(1+ILOCB)/256. + 0.1
            IA2 = RPARM(1+ILOCB) - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         IF ((IA1.LE.0) .OR. (IA1.GT.MAXANT)) DOIT = .TRUE.
         IF ((IA2.LE.0) .OR. (IA2.GT.MAXANT)) DOIT = .TRUE.
C                                       If selecting by baseline
         IF ((DOBLIN) .AND. (.NOT.DOIT)) THEN
            IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT, NANTS, IBAS, NBASS))
     *         GO TO 100
            END IF
C                                       scaling info
         IF (DOSCAL.GT.0.0) THEN
            ISOU = 0
            IF (ILOCSU.GE.0) ISOU = IROUND (RPARM(1+ILOCSU))
            IF (ISOU.LE.0) ISOU = INSNUM
            IF (ISOU.NE.LSOU) THEN
               LSOU = ISOU
               CALL GETSOU (LSOU, DISK, CNO, CATUV, NXLUN, IRET)
               DO 105 LIF = BIF,EIF
                  IF (FLUX(1,LIF).LE.1.E-10) FLUX(1,LIF) = 1.0
 105              CONTINUE
               IF (DOSCAL.GT.1.5) THEN
                  K = DOSCAL - 0.5
                  CALL FNDSPX (DISK, CNO, LSOU, FRQSEL, CATUV, K, SPIX,
     *               IRET)
               ELSE
                  SPIX(2) = 0.0
                  END IF
               END IF
C                                       Get and scale vis
            CALL UVFNDS (FLUX, SPIX, NPOL, VIS)
            END IF
C                                       Convert uvw to kilo lamda.
         U = RPARM(1+ILOCU) * 0.001 * UVSCAL
         V = RPARM(1+ILOCV) * 0.001 * UVSCAL
         W = RPARM(1+ILOCW) * 0.001 * UVSCAL
         UVM = MAX (UVM, U)
         UVM = MAX (UVM, V)
         UVM = MAX (UVM, W)
         UVN = U
         UVN = MIN (UVN, V)
         UVN = MIN (UVN, W)
         UVM = MAX (UVM, -10.0*UVN)
         R = U * U + V * V
         IF ((.NOT.DOIT) .AND. (XWTUV.GT.0.00001)) THEN
            INDEX = 1
            DO 115 KK = BCHAN,ECHAN
               DO 110 KKK = 1,NCOR
                  DOIT = DOIT .OR. (VIS(3,INDEX).GT.XWTUV)
                  INDEX = INDEX + 1
 110              CONTINUE
 115           CONTINUE
            END IF
C                                       average up the data if needed
         IF (NCHAV.GT.1) THEN
            K = 0
            DO 135 KK = BCHAN,ECHAN,NCHAV
               CALL RFILL (4, 0.0, XA)
               CALL RFILL (4, 0.0, YA)
               CALL RFILL (4, 0.0, WA)
               DO 125 LL = KK,MIN(ECHAN,KK+NCHAV-1)
                  DO 120 KKK = 1,NCOR
                     K = K + 1
                     INDEX = ((LL-BCHAN)*INCF + (KKK-1)*INCS) / 3 + 1
                     IF (VIS(3,INDEX).GT.0.0) THEN
                        XA(KKK) = XA(KKK) + VIS(1,INDEX)*VIS(3,INDEX)
                        YA(KKK) = YA(KKK) + VIS(2,INDEX)*VIS(3,INDEX)
                        WA(KKK) = WA(KKK) + VIS(3,INDEX)
                        END IF
 120                 CONTINUE
 125              CONTINUE
               DO 130 KKK = 1,NCOR
                  INDEX = ((KK-BCHAN)*INCF + (KKK-1)*INCS) / 3 + 1
                  IF (WA(KKK).GT.0.0) THEN
                     VIS(1,INDEX) = XA(KKK) / WA(KKK)
                     VIS(2,INDEX) = YA(KKK) / WA(KKK)
                     VIS(3,INDEX) = WA(KKK)
                  ELSE
                     VIS(1,INDEX) = 0.0
                     VIS(2,INDEX) = 0.0
                     VIS(3,INDEX) = 0.0
                     END IF
 130              CONTINUE
 135           CONTINUE
            END IF
C                                       UV annulus
         IF (.NOT.DOIT) THEN
            IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 100
            IF (DOUVPI) THEN
               IF ((R.GE.RADIUS(1)) .AND. (R.LE.RADIUS(2))) THEN
                  P = 90.0
                  IF (V.NE.0.0) P = ATAN(U/V) * RADEG
                  IF (((P.GE.POSANG(1)) .AND. (P.LE.POSANG(2))) .OR.
     *               ((P+180.GE.POSANG(1)) .AND. (P+180.LE.POSANG(2))))
     *               DOIT = .TRUE.
                  END IF
C                                       UV box
            ELSE IF (DOUVBX) THEN
               IF ((U.GE.RADIUS(1)) .AND. (U.LE.RADIUS(2)) .AND.
     *            (V.GE.POSANG(1)) .AND. (V.LE.POSANG(2))) THEN
                  DOIT = .TRUE.
C                                       Hermitian
               ELSE IF (APARM(5).GT.0.0) THEN
                  IF ((-U.GE.RADIUS(1)) .AND. (-U.LE.RADIUS(2)) .AND.
     *               (-V.GE.POSANG(1)) .AND. (-V.LE.POSANG(2)))
     *               DOIT = .TRUE.
                  END IF
C                                       Do we want this one
            ELSE IF ((DOMAX) .OR. (DOVCLP)) THEN
               K = 0
               DO 160 II = BCHAN,ECHAN,NCHAV
                  DO 155 JJ = 1,NCOR
                     K = K + 1
                     IF (VIS(3,K).GT.0.0) THEN
                        XX = VIS(1,K) * VIS(1,K) + VIS(2,K) * VIS(2,K)
                        YY = 0.0
                        IF (XX.GT.0.0) YY = ATAN2 (VIS(2,K), VIS(1,K))
                        IF (DOVCLP) THEN
                           IF (XX.GT.APARM(1)) DOIT = .TRUE.
                        ELSE
                           TEMP = CATD(KDCRV+JLOCS) + CATR(KRCIC+JLOCS)
     *                        * (JJ - CATR(KRCRP+JLOCS))
                           I = IROUND (TEMP)
                           L = 2
                           IF ((I.GE.-2) .AND. (I.LE.1)) L = 1
                           IF ((I.GE.-6) .AND. (I.LE.-5)) L = 1
                           IF (XX.GT.APARM(L)) DOIT = .TRUE.
                           IF (XX.LT.APARM(L+2)) DOIT = .TRUE.
                           L = L + 4
                           IF (YY.GT.APARM(L)) DOIT = .TRUE.
                           IF (YY.LT.APARM(L+2)) DOIT = .TRUE.
                           END IF
                        IF (DOIT) GO TO 165
                        END IF
 155                 CONTINUE
 160              CONTINUE
               END IF
            END IF
C                                       Use this point
 165     IF (DOIT) THEN
C                                       New source?
            IF (MULTI) THEN
               SOUID = RPARM(1+ILOCSU) + 0.5
               IF (SOUID.NE.OLDSID) THEN
                  CALL SOURS (SOUID, OLDSID, OTYPE, IERR)
                  IF (IERR.NE.0) GO TO 900
                  OLDSID = SOUID
                  END IF
               END IF
C                                       Get vis.
            K = 0
            YAMP = 0.0
            NOK = 0
            DO 170 KK = ICH1,ICH2,NCHAV
               DO 170 KKK = 1,NCOR
                  K = K + 1
                  INDEX = ((KK-BCHAN)*INCF + (KKK-1)*INCS) / 3 + 1
                  XX = VIS(1,INDEX)
                  YY = VIS(2,INDEX)
                  TPHS = VIS(3,INDEX) * WTSC
                  TPHS = MAX (-99.0, MIN (99.0, TPHS))
                  IWT(K) = IROUND (TPHS)
                  IF (TPHS.GT.0.0) NOK = NOK + 1
                  IF (IA1.NE.IA2) THEN
                     AMP(K) = SQRT (XX*XX+YY*YY)
                  ELSE
                     AMP(K) = XX
                     END IF
                  XAMP = MAX (XAMP, AMP(K))
                  YAMP = MIN (YAMP, AMP(K))
                  TPHS = 57.296 * ATAN2 (YY, XX+1.0E-20)
                  PHASE(K) = IROUND (TPHS)
 170              CONTINUE
            IF (NOK.LE.0) GO TO 100
            IF (IAPCNT.EQ.NITER) GO TO 200
            IAPCNT = IAPCNT + 1
C                                       Write VIS data: CRT
            XAMP = MAX (XAMP, -10.0*YAMP)
C                                       low UVW, low flux
            IF ((XAMP.GT.99.9) .AND. (OTYPE.GT.2)) GO TO 180
               IF ((UVM.GT.9999.98) .AND. (OTYPE.LT.5)) GO TO 175
                  IF (OTYPE.EQ.1) WRITE (LINE,1140,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V, W,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.2) WRITE (LINE,1141,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.3) WRITE (LINE,1142,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.4) WRITE (LINE,1143,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.5) WRITE (LINE,1144,ERR=190)
     *               XCOUNT, ITT, IA1, IA2,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.6) WRITE (LINE,1145,ERR=190)
     *               ITT(2), ITT(3), ITT(4), IA1, IA2,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  GO TO 190
C                                       High UVW, low flux
 175           CONTINUE
                  IF (OTYPE.EQ.1) WRITE (LINE,1146,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V, W,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.2) WRITE (LINE,1147,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.3) WRITE (LINE,1148,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.4) WRITE (LINE,1149,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  GO TO 190
C                                       low UVW, higher flux
 180        CONTINUE
               IF ((UVM.GT.9999.98) .AND. (OTYPE.LT.5)) GO TO 185
                  IF (OTYPE.EQ.3) WRITE (LINE,1150,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.4) WRITE (LINE,1151,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.5) WRITE (LINE,1152,ERR=190)
     *               XCOUNT, ITT, IA1, IA2,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.6) WRITE (LINE,1153,ERR=190)
     *               ITT(2), ITT(3), ITT(4), IA1, IA2,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  GO TO 190
C                                       High UVW, higher flux
 185           CONTINUE
                  IF (OTYPE.EQ.3) WRITE (LINE,1155,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.4) WRITE (LINE,1156,ERR=190)
     *               XCOUNT, ITT, IA1, IA2, U, V,
     *               (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
 190        CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
      GO TO 100
C                                       Number lines printed
 200  IF (DOCRT.LE.0.0) THEN
         WRITE (LINE,1200) IAPCNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1201) IAPCNT
      CALL MSGWRT (4)
      IF ((IAPCNT.GE.NITER) .AND. (NITER.NE.NVIS)) THEN
         IF (DOCRT.LE.0.0) THEN
            WRITE (LINE,1202)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            END IF
         WRITE (MSGTXT,1203)
         CALL MSGWRT (6)
         END IF
C                                       Close files.
 900  IF (IERR.LE.0) IRET = 0
      CALL LPCLOS (PLUN, PIND, IPCNT, IERR)
C
      CALL UVGET ('CLOS', RPARM, VIS, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('File= ',A12,'.',A6,'.',I4,' Vol=',I2,' Userid=',
     *   I5,' Channels=',I5,' to',I5,' by',I4,' IF=',I3,' Sub=',I3)
 1021 FORMAT (A12,'.',A6,'.',I4,I3,' User=',I5,' Chans=',I4,
     *   ' to',I4,' by',I3,' IF=',I3,' Sub=',I3)
 1022 FORMAT ('Source= ',A8,3X,A4,' = ',A1,I2.2,I3.2,F6.2,3X,A4,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1023 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
 1024 FORMAT ('Weights have been multiplied by',F12.4)
 1040 FORMAT ('Source= ',A8,4X,'Freq= ',F13.9,4X,'Sort= ',A2,1X,
     *   5(4X,I4,1X,A2,4X))
 1041 FORMAT (A,4X,'Freq=',F13.9,4X,'Sort= ',A2,1X,5(4X,I4,1X,A2,4X))
 1042 FORMAT (A,4X,'Freq=',F13.9,4X,'Sort= ',A2,1X,4(3X,I4,1X,A2,4X))
 1043 FORMAT (A,4X,'Freq=',F13.9,4X,'Sort= ',A2,4(3X,I4,1X,A2,4X))
 1044 FORMAT (A,F13.9,1X,A2,4(3X,I4,1X,A2,4X))
 1045 FORMAT (A,2X,A2,2X,4(3X,I4,1X,A2,4X))
 1050 FORMAT ('  Vis #',5X,'IAT',6X,'Ant   U(klam)  V(klam)  W(klam)',
     *   5(3X,A12))
 1051 FORMAT ('  Vis #',5X,'IAT',6X,'Ant   U(klam)  V(klam)',5(3X,A12))
 1052 FORMAT ('  Vis #',5X,'IAT',6X,'Ant   U(klam)  V(klam)',4(2X,A12))
 1053 FORMAT (' Vis #',5X,'IAT',6X,'Ant   U(klam)  V(klam)',4(2X,A12))
 1054 FORMAT (' Vis #',5X,'IAT',6X,'Ant ',4(2X,A12))
 1055 FORMAT (2X,'IAT',5X,'Ant ',4(2X,A12))
 1140 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,3F9.2,5(F8.3,I4,I3))
 1141 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,5(F8.3,I4,I3))
 1142 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.3,I4,I3))
 1143 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.3,I4,I3))
 1144 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,I4,I3))
 1145 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,I4,I3))
 1146 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,3F9.0,5(F8.3,I4,I3))
 1147 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,5(F8.3,I4,I3))
 1148 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.3,I4,I3))
 1149 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.3,I4,I3))
 1150 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.1,I4,I3))
 1151 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.1,I4,I3))
 1152 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,I4,I3))
 1153 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,I4,I3))
 1155 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.1,I4,I3))
 1156 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.1,I4,I3))
 1200 FORMAT (I10,' Points found')
 1201 FORMAT (I10,' Points printed')
 1202 FORMAT ('********** Print limit reached: more points may exist',
     *   ' **********')
 1203 FORMAT ('WARNING: PRINT LIMIT REACHED, NOT ALL POINTS LISTED')
      END
      SUBROUTINE UVFNDS (FLUX, SPIX, NPOL, BUFR)
C-----------------------------------------------------------------------
C   Scales data buffer by flux and spectral index
C   Inputs:
C      FLUX   R(4,*)   Flux by IF
C      SPIX   R(3)     Flux at 1 GHz, spectral index, curvatur
C                         spix(1): < -1000 no scale
C   In/out:
C      BUFR   R(3,*)   data buffer
C-----------------------------------------------------------------------
      INTEGER   NPOL
      REAL      FLUX(4,*), SPIX(3), BUFR(*)
C
      INTEGER   LF, LC, LP, NCHAN, LAD
      REAL      CATUVR(256), SCALE, DEN
      DOUBLE PRECISION CATUVD(128), REFREQ, REFPIX
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATUVD, CATUVR)
C-----------------------------------------------------------------------
      IF (SPIX(2).GT.-1000.) THEN
         REFREQ = CATUVD(KDCRV+JLOCF)
         REFPIX = CATUVR(KRCRP+JLOCF)
         NCHAN = CATUV(KINAX+JLOCF)
         DO 20 LF = BIF,EIF
            SCALE = 1.0 / FLUX(1,LF)
            IF (SPIX(2).NE.0.0) DEN = (REFREQ + FOFF(LF) +
     *         FINC(LF) * (NCHAN/2.0 - REFPIX)) / 1.D9
            DO 15 LC = BCHAN,ECHAN
C                                       scale w spectral index
               IF (SPIX(2).NE.0.0) THEN
                  SCALE = ((REFREQ + FOFF(LF) + FINC(LF) *
     *               (LC - REFPIX)) / 1.D9)
                  IF ((SPIX(3).EQ.0.0) .OR. (SPIX(1).LE.0.0)) THEN
                     SCALE = (SCALE / DEN) ** SPIX(2)
                     SCALE = 1.0 / (SCALE * FLUX(1,LF))
                  ELSE
                     SCALE = LOG10 (SCALE)
                     SCALE = SPIX(2)*SCALE + SPIX(3)*SCALE*SCALE
                     SCALE = 1.0 / (SPIX(1) * (10.0 ** SCALE))
                     END IF
                  END IF
               DO 10 LP = 1,NPOL
                  LAD = (LC - BCHAN) * INCF + (LF - BIF) * INCIF +
     *               (LP - 1) * INCS
                  IF (BUFR(LAD+3).GT.0) THEN
                     BUFR(LAD+1) = BUFR(LAD+1) * SCALE
                     BUFR(LAD+2) = BUFR(LAD+2) * SCALE
                     BUFR(LAD+3) = BUFR(LAD+3) / SCALE / SCALE
                     END IF
 10               CONTINUE
 15            CONTINUE
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SOURS (SOUID, OLDSID, OTYPE, IERR)
C-----------------------------------------------------------------------
C   Process the next source number adding header info.
C   Calls GETSOU to fill in commons with source info.
C   Input:
C      SOUID   I      Source ID number
C      OTYPE   I      Output width type.
C   Input from common:
C      NACROS  I      Actual output width.
C      DISK    I      Input file disk number.
C      CNO     I      Input file catalog slot number
C      PLUN    I      LUN for output.
C      PIND    I      FTAB pointer for output.
C   Input/output from common:
C      IPCNT   I      Line count on page
C      PAGE    I      Page number
C      TITL1   C*132  First title line
C      TITL2   C*132  Second title line
C   Output:
C      IERR    I      Return error code, 0=>OK else failed
C   Output in common:
C      All values in DSOU.INC
C-----------------------------------------------------------------------
      INTEGER   SOUID, OLDSID, OTYPE, IERR
C
      INTEGER   SULUN
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFND.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA SULUN /17/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get new source info
      CALL GETSOU (SOUID, DISK, CNO, CATOLD, SULUN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, SOUID
         GO TO 990
         END IF
C                                       Change source name in TITL1
      IF (OTYPE.EQ.1) THEN
         TITL1(9:16) = SNAME(1:8)
      ELSE
         TITL1(1:8) = SNAME(1:8)
         END IF
C                                       Print titles for new source.
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
      IF ((DOCRT.GT.-2.5) .OR. (OLDSID.LT.0)) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      GO TO 999
C                                       Error writing output
 950  WRITE (MSGTXT,1950) IERR
C                                       Error
 990  IF (IERR.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR:',I7,' OBTAINING DATA FOR SOURCE #',I5)
 1950 FORMAT ('ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE FNDSPR (BCHAN, ECHAN, IRET)
C-----------------------------------------------------------------------
C   FNDSPR reads the singledish file and prints those samples meeting
C   the criteria given by OPCODE and APARM.
C   Routine to handle single dish data.
C   Output:
C      IRET   I   Return code: 0 success or user requests end,
C                    else bad
C-----------------------------------------------------------------------
      INTEGER   BCHAN, ECHAN, IRET
C
      CHARACTER CHSCAN*8, CHSAMP*8,  LLCH*4, MMCH*4, JSTOKE(4,2)*2,
     *   TCHAR(2)*4, CH1*1, CH2*1, ISTOKE(8)*2
      LOGICAL   DOIT, NORMAL, ISPROJ
      INTEGER   ICH(8), CINC, HM(2), DD(2), ITT(4), IWT(6), INDEX,
     *   IROUND, I, IBEAM, ICH1, ICH2, IERR, L, II, JJ, KK, LL,
     *   KKK, JCOR, LIMIT2, NNCH, NUMCH, K, NUMCHM, OTYPE, IRAS, IDECS,
     *   ILOCSM, ILOCSC, IOPC, XCOUNT, COUNT, IAPCNT, SCAN, SAMPLE
      REAL   RSEC, DSEC, AMP(6), TEMP, TPHS, U, V, RADEG, XX, UVN,
     *   UVSCAL, XDAY, WTSC, XA(4), YA(4), WA(4)
      DOUBLE PRECISION    XRA, XDEC
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'UVFND.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CHSCAN, CHSAMP /'SCAN    ','SAMP    '/
      DATA JSTOKE /'R ','L ','??','??', 'I ','Q ','U ','V '/
      DATA TCHAR /'FLUX','WT  '/
C-----------------------------------------------------------------------
      IRET = 8
      CALL SDGET ('INIT', RPARM, VIS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Single dish stuff
      NORMAL = IERR.EQ.0
      ISPROJ = TYPUVD.EQ.2
      UVSCAL = 1.0 / FREQ
      IF (NORMAL) UVSCAL = 1.0
      IOPC = 1
      IF (ISPROJ) IOPC = 2
      IF (.NOT.NORMAL) ILOCB = 2
      IBEAM = 1
      SAMPLE = 0
      SCAN = 0
C                                       SCAN pointer
      CALL AXEFND (4, CHSCAN, CATBLK(KIPCN), CATR(KHPTP), ILOCSC, IERR)
      IF (IERR.NE.0) ILOCSC = -1
C                                       SAMPLE pointer
      CALL AXEFND (4, CHSAMP, CATBLK(KIPCN), CATR(KHPTP), ILOCSM, IERR)
      IF (IERR.NE.0) ILOCSM = -1
      RADEG = 180.0 / 3.14159
      CINC = 2
C                                       range parameters
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.9995)
      I = TEMP + 98.0
      I =  100 - I
      WTSC = 10.0 ** I
      IF ((XWT.LT.99.5) .AND. (NWT.GE.1.0)) WTSC = 10.0
      IF ((XWT.LT.999.5) .AND. (NWT.GE.10.0)) WTSC = 1.0
      TEMP = NWT * WTSC
      IF (TEMP.LT.1.0) THEN
         MSGTXT = 'Full dynamic range of weights cannot be printed'
         CALL MSGWRT (6)
         END IF
C                                        Determine no. channels to do
      FREQ = FREQ - (CATR(KRCRP+JLOCF)-NCH) * CATR(KRCIC+JLOCF)
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      NUMCH = (NACROS - 52) / (13 * NCOR)
      NUMCH = MIN (NUMCH, NUMCHM)
      NUMCH = MIN (NUMCH, 6)
      OTYPE = 1
      IF (NUMCH.LT.1) THEN
         NUMCH = 1
         IF (NACROS.LE.80) OTYPE = 2
         IF (NACROS.LE.72) OTYPE = 3
         END IF
      LIMIT2 = NCOR * NUMCH
      ICH1 = NCH
      ICH2 = NCH + (NUMCH - 1) * NCHAV
      IF (ICH2.GT.ECHAN) ICH2 = ECHAN
      NNCH = NCH - 1
      DO 10 II = 1,LIMIT2,NCOR
         NNCH = NNCH + 1
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
            ISTOKE(INDEX) = JSTOKE(I,L)
 10         CONTINUE
C                                       Convert coordinates.
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*CINC), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*CINC), MMCH)
      IF (LLCH(:2).EQ.'RA') THEN
         CALL COORDD (1, RA, CH1, HM, RSEC)
      ELSE
         CALL COORDD (2, RA, CH1, HM, RSEC)
         END IF
      CALL COORDD (2, DEC, CH2, DD, DSEC)
      FREQ = FREQ * 1.0E-9
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write first page header
      IF (NACROS.GE.90) THEN
         WRITE (LINE,1020) NAME, CLASS, SEQ, DISK, USERID, BCHAN, ECHAN,
     *      NCHAV, IBIF
      ELSE
         WRITE (LINE,1021) NAME, CLASS, SEQ, DISK, USERID, BCHAN, ECHAN,
     *      NCHAV, IBIF
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE, IPCNT,
     *   PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (LINE,1022) SOURCE, LLCH, CH1, HM, RSEC, MMCH, CH2, DD, DSEC
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE, IPCNT,
     *   PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (LINE,1023) FREQ, NCOR, NVIS, ISORT
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE, IPCNT,
     *   PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (WTSC.NE.1.0) THEN
         WRITE (LINE,1024) WTSC
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Page titles
      IF (OTYPE.EQ.1) THEN
         WRITE (TITL1,1040) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         WRITE (TITL2,1050) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.2) THEN
         WRITE (TITL1,1041) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         WRITE (TITL2,1051) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.3) THEN
         WRITE (TITL1,1042) SOURCE, FREQ, (ICH(JCOR), ISTOKE(JCOR),
     *      JCOR = 1,LIMIT2)
         WRITE (TITL2,1052) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      LINE = ' '
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      COUNT = 0
      XCOUNT = 0
      PAGE = 1
      IAPCNT = 0
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         COUNT = COUNT + 1
         CALL SDGET ('READ', RPARM, VIS, IERR)
C                                       Done?
         IF (IERR.EQ.-1) THEN
            IERR = 0
            GO TO 200
            END IF
         IF (IERR.NE.0) GO TO 999
C                                       Update counters
         XCOUNT = XCOUNT + 1
         DOIT = .FALSE.
C                                       Decode time.
         XDAY = RPARM(1+ILOCT)
         IF ((XDAY.LT.0.0) .OR. (XDAY.GT.1000.)) DOIT = .TRUE.
         CALL TODHMS (XDAY, ITT)
C                                       Scan, Sample
         IF (ILOCSC.GE.0) SCAN = IROUND (RPARM(1+ILOCSC))
         IF (ILOCSM.GE.0) SAMPLE = IROUND (RPARM(1+ILOCSM))
         IF ((SCAN.LT.0) .OR. (SAMPLE.LT.0)) DOIT = .TRUE.
C                                       Determine beam
         IF (ILOCB.GE.0) THEN
            IBEAM = RPARM(1+ILOCB) + 0.1
            IF (IBEAM.GT.256) IBEAM = MOD (IBEAM, 256)
         ELSE
            IBEAM = RPARM(1+ILOCA2) + 0.1
            END IF
         IF (IBEAM.LE.0) DOIT = .TRUE.
C                                       RA, Dec. Range
         U = RPARM(1+ILOCU)
         V = RPARM(1+ILOCV)
         IF ((U.LT.-361.) .OR. (U.GT.361.)) DOIT = .TRUE.
         IF ((V.LT.-90.1) .OR. (V.GT.90.1)) DOIT = .TRUE.
         UVM = MAX (UVM, U)
         UVM = MAX (UVM, V)
         UVN = MIN (U, V)
         UVM = MAX (UVM, -10.0*UVN)
C                                       check weights
         IF ((XWTUV.GT.0.00001) .AND. (.NOT.DOIT)) THEN
            INDEX = 1
            DO 115 KK = BCHAN,ECHAN
               DO 110 KKK = 1,NCOR
                  DOIT = DOIT .OR. (VIS(3,INDEX).GT.XWTUV)
                  INDEX = INDEX + 1
 110              CONTINUE
 115           CONTINUE
            END IF
C                                       channel averaging
         IF (NCHAV.GT.1) THEN
            K = 0
            DO 135 KK = BCHAN,ECHAN,NCHAV
               CALL RFILL (4, 0.0, XA)
               CALL RFILL (4, 0.0, YA)
               CALL RFILL (4, 0.0, WA)
               DO 125 LL = KK,MIN(ECHAN,KK+NCHAV-1)
                  DO 120 KKK = 1,NCOR
                     K = K + 1
                     INDEX = ((LL-BCHAN)*INCF + (KKK-1)*INCS) / 3 + 1
                     IF (VIS(3,INDEX).GT.0.0) THEN
                        XA(KKK) = XA(KKK) + VIS(1,INDEX)*VIS(3,INDEX)
                        YA(KKK) = YA(KKK) + VIS(2,INDEX)*VIS(3,INDEX)
                        WA(KKK) = WA(KKK) + VIS(3,INDEX)
                        END IF
 120                 CONTINUE
 125              CONTINUE
               DO 130 KKK = 1,NCOR
                  INDEX = ((KK-BCHAN)*INCF + (KKK-1)*INCS) / 3 + 1
                  IF (WA(KKK).GT.0.0) THEN
                     VIS(1,INDEX) = XA(KKK) / WA(KKK)
                     VIS(2,INDEX) = YA(KKK) / WA(KKK)
                     VIS(3,INDEX) = WA(KKK)
                  ELSE
                     VIS(1,INDEX) = 0.0
                     VIS(2,INDEX) = 0.0
                     VIS(3,INDEX) = 0.0
                     END IF
 130              CONTINUE
 135           CONTINUE
            END IF
C                                       UV box
         IF ((DOUVBX) .AND. (.NOT.DOIT)) THEN
            DOIT = (U.GE.RADIUS(1)) .AND. (U.LE.RADIUS(2)) .AND.
     *         (V.GE.POSANG(1)) .AND. (V.LE.POSANG(2))
            END IF
C                                       Do we want this one
         IF (((DOMAX) .OR. (DOVCLP)) .AND. (.NOT.DOIT)) THEN
            K = 0
            DO 170 II = BCHAN,ECHAN,NCHAV
               DO 165 JJ = 1,NCOR
                  K = K + 1
                  IF (VIS(3,K).GT.0.0) THEN
                     XX = VIS(1,K) * VIS(1,K) + VIS(2,K) * VIS(2,K)
                     IF (DOVCLP) THEN
                        IF (XX.GT.APARM(1)) DOIT = .TRUE.
                     ELSE
                        TEMP = CATD(KDCRV+JLOCS) + CATR(KRCIC+JLOCS) *
     *                     (JJ - CATR(KRCRP+JLOCS))
                        I = IROUND (TEMP)
                        L = 2
                        IF ((I.GE.-2) .AND. (I.LE.1)) L = 1
                        IF ((I.GE.-6) .AND. (I.LE.-5)) L = 1
                        IF (XX.GT.APARM(L)) DOIT = .TRUE.
                        IF (XX.LT.APARM(L+2)) DOIT = .TRUE.
                        END IF
                     IF (DOIT) GO TO 175
                     END IF
 165              CONTINUE
 170           CONTINUE
            END IF
C                                       Print it already
 175     IF (DOIT) THEN
C                                       Convert RA, Dec.
            XRA = RPARM(1+ILOCU) * UVSCAL
            XDEC = RPARM(1+ILOCV) * UVSCAL
            CALL COORDD (IOPC, XRA, CH1, HM, RSEC)
            CALL COORDD (2, XDEC, CH2, DD, DSEC)
            IRAS = IROUND (RSEC)
            IDECS = IROUND (DSEC)
C                                       Get data.
            K = 0
            DO 185 KK = ICH1,ICH2,NCHAV
               DO 180 KKK = 1,NCOR
                  K = K + 1
                  INDEX = ((KK-BCHAN)*INCF + (KKK-1)*INCS) / 3 + 1
                  XX = VIS(1,INDEX)
                  TPHS = VIS(3,INDEX) * WTSC
                  TPHS = MAX (-999.0, MIN (999.0, TPHS))
                  IWT(K) = IROUND (TPHS)
                  AMP(K) = XX
 180              CONTINUE
 185           CONTINUE
            IF (IAPCNT.EQ.NITER) GO TO 200
            IAPCNT = IAPCNT + 1
C                                       Write data:
            IF (OTYPE.EQ.1) THEN
               WRITE (LINE,1140,ERR=190) SCAN, SAMPLE, ITT, IBEAM, CH1,
     *            HM, IRAS, CH2, DD, IDECS, (AMP(K), IWT(K),
     *            K = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.2) THEN
               WRITE (LINE,1141,ERR=190) SCAN, SAMPLE, IBEAM, CH1, HM,
     *            IRAS, CH2, DD, IDECS, (AMP(K), IWT(K), K = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.3) THEN
               WRITE (LINE,1142,ERR=190) CH1, HM, IRAS, CH2, DD, IDECS,
     *            (AMP(K), IWT(K), K = 1,LIMIT2)
               END IF
 190        CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         GO TO 100
C                                       Number lines printed
 200  IF (DOCRT.LE.0.0) THEN
         WRITE (LINE,1200) IAPCNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1201) IAPCNT
      CALL MSGWRT (4)
      IF ((IAPCNT.GE.NITER) .AND. (NITER.NE.NVIS)) THEN
         IF (DOCRT.LE.0.0) THEN
            WRITE (LINE,1202)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            END IF
         WRITE (MSGTXT,1203)
         CALL MSGWRT (6)
         END IF
C                                       Close files.
 900  IF (IERR.LE.0) IRET = 0
      CALL LPCLOS (PLUN, PIND, IPCNT, BUFF, IERR)
C
      CALL SDGET ('CLOS', RPARM, VIS, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('File= ',A12,'.',A6,'.',I4,'  Vol =',I2,'   Userid =',I5,
     *   5X,'Channels =',I5,' to',I5,' by',I4,' IF=',I3)
 1021 FORMAT (A12,'.',A6,'.',I4,I2,'  User=',I5,'  Chans=',I4,' to ',
     *   I4,' by',I4,'  IF=',I3)
 1022 FORMAT ('Source= ',A8,3X,A4,' = ',A1,I2.2,I3.2,F6.2,3X,A4,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1023 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
 1024 FORMAT ('Weights have been multiplied by',F12.4)
 1040 FORMAT ('Source=',A8,7X,'Freq=',F13.9,4X,'Sort=',A2,
     *   6(3X,I4,2X,A2,2X))
 1041 FORMAT (A,4X,F13.9,2X,A2,6X,5(3X,I4,2X,A2,2X))
 1042 FORMAT (A,F12.8,4(3X,I4,2X,A2,2X))
 1050 FORMAT (' Scan/Samp',6X,'IAT',4X,'Beam ',6X,'RA',8X,'Dec',3X,
     *   6(4X,A4,3X,A2))
 1051 FORMAT (' Scan/  # Beam',6X,'RA',7X,'Dec',3X,4(4X,A4,3X,A2))
 1052 FORMAT (5X,'RA',7X,'Dec',3X,4(4X,A4,3X,A2))
 1140 FORMAT (I5,'/',I4.4,I4,'/',I2.2,2(':',I2.2),I4,1X,
     *   2(2X,A1,I2.2,2(':',I2.2)),6(F9.2,I4))
 1141 FORMAT (I5,'/',I4.4,I4,1X,2(1X,A1,I2.2,2(':',I2.2)),6(F9.2,I4))
 1142 FORMAT (2(1X,A1,I2.2,2(':',I2.2)),6(F9.2,I4))
 1200 FORMAT (I10,' Points found')
 1201 FORMAT (I10,' Points printed')
 1202 FORMAT ('********** Print limit reached: more points may exist',
     *   ' **********')
 1203 FORMAT ('WARNING: PRINT LIMIT REACHED, NOT ALL POINTS LISTED')
 1900 FORMAT ('ERROR:',I7,' CLOSING UV FILE ')
      END
