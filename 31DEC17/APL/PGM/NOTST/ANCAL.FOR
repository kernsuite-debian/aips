LOCAL INCLUDE 'ANCAL.INC'
C                                                          Include ANCAL
C                                       Local include for ANCAL
C                                       Needs parameter from PUVD.INC
      INTEGER MAXSRC
      PARAMETER (MAXSRC=500)
C                                       MXTANT is the maximum number of
C                                       antenna temperature entries.
      INTEGER MXTANT
      PARAMETER (MXTANT=20000)
C                                       Inputs and general info
      INTEGER   SEQIN, SUBA, DISKIN, NUMHIS, LUNK, FINDK, DISKCL, CNOCL,
     *   CLVER, BLVER, BIF, EIF, POLNO, ITYRNO, FREQID, NUMANT, NUMPOL,
     *   NUMIF
      REAL      XSIN, XDISIN, XSUBA, XGVER, XBVER, XBAND, XFREQ,
     *   XFQID, XBAD(10), SELBAN
      DOUBLE PRECISION SELFRQ
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(12)
      CHARACTER HISCRD(10)*64, NAMEIN*12, CLAIN*6, NAME2*48
C                                       Source info
      INTEGER   REFDAY, YEAR, NAXIS, NCMPLX, NSTOKE, NFREQS
      CHARACTER SUNAM(MAXSRC)*8
      REAL      TFLUX(MAXSRC)
C                                       Gain info
      INTEGER   NTANT(MAXANT), NTSYS(MAXANT), IGAIN(MAXANT), ISTART,
     *    NSKPDY, NSTEPS, NDAYS(20), SKIPDY(20)
      REAL      GACOEF(10,MAXANT), GAOFFS(MAXANT), DPFU(MAXANT),
     *   TOFFS(2,MAXANT), TDPFU(2,MAXANT),
     *   TANTS(2,MAXANT,MXTANT), BFAC(MAXANT,MAXANT),  BDEF
      LOGICAL   ISTATS(MAXANT), DOBFAC
C                                       Buffers and file info
      INTEGER   DISKSC, CNOSC, LUNSC, INDSC, IBPSC, STBUFF(1024),
     *   SCBUFF(512), SXBUFF(512), ITYVER
      DOUBLE PRECISION DUMMY(100000)
C                                       Important constants
      DOUBLE PRECISION PI, TWOPI, SIDER, CLIGHT
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAME2, XSUBA,
     *   XGVER, XBVER, XBAND, XFREQ, XFQID, XBAD, SEQIN, DISKIN, SUBA,
     *   CLVER, BLVER, BIF, EIF, POLNO, FREQID, NUMANT, NUMPOL, NUMIF
      COMMON /KEYCOM/ SELFRQ, SELBAN, ITYRNO, LUNK, FINDK, DISKCL,
     *   CNOCL, NUMHIS
C                                       CHARACTER info
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAME2, SUNAM, HISCRD
C                                       Source info
      COMMON /SRXDAT/ REFDAY, YEAR, NAXIS, NCMPLX, NSTOKE, NFREQS
C                                       Gain info
      COMMON /CALIB/ GACOEF, GAOFFS, DPFU, TOFFS, TDPFU, TANTS, BFAC,
     *   TFLUX, BDEF, NTANT, NTSYS, IGAIN, ISTART,
     *   SKIPDY, NDAYS, NSKPDY, NSTEPS, ISTATS, DOBFAC
C                                       Buffers and file info
      COMMON /SCFILE/ DUMMY, STBUFF, SCBUFF, SXBUFF, DISKSC, CNOSC,
     *   LUNSC, INDSC, IBPSC, ITYVER
C                                       Important constants
      COMMON /CONST/ PI, TWOPI, SIDER, CLIGHT
C                                                          End ANCAL
LOCAL END
LOCAL INCLUDE 'ANCAL2.INC'
C                                       TY table stuff
      INTEGER   ANT(2), TYKOLS(MAXTYC), TYNUMV(MAXTYC), NUMTY
      REAL      XTIME(2), XTSYS(2,MAXIF,2), XTANT(2,MAXIF,2)
      COMMON /TYCOM/ XTIME, XTSYS, XTANT, NUMTY, ANT, TYKOLS, TYNUMV
LOCAL END
      PROGRAM ANCAL
C-----------------------------------------------------------------------
C! VLBI amplitude calibration from antenna and system temperatures etc.
C# UV Calibration EXT-util VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2003, 2007, 2011, 2014-2015
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
C   Task ANCAL computes user supplied gain calibrations to VLBI
C   visibility amplitudes. A table of T sys's and gain curves is read
C   from the AIPS runfile directory.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      IN2NAME        NAME2         Name of aux. file.
C      SUBARRAY       SUBA          Specified  subarray.
C      GAINVER        CLVER         CL table to update
C      BADDISK        IBAD          Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'ANCAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL ANCLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read gain curves, antenna
C                                       temps, flux, b-factors, T sys's,
C                                       etc. Update CL table.
      CALL GCURVE (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update TY table
      CALL TYCONS (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       BL table if necessary
      IF (DOBFAC) CALL BLFAC (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL ANCLHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, STBUFF)
 999  STOP
      END
      SUBROUTINE ANCLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   ANCLIN gets input parameters for ANCAL.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, CHTM8*8, UTYPE*2
      LOGICAL   T, F, MATCH
      INTEGER   JERR, NPARM, IERR, I
      INTEGER   OLDCNO, IROUND, IT(6), LUN
      REAL      BANDW
      DOUBLE PRECISION JD
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      NUMHIS = 0
      LUNK = 10
C                                       Set important constants
      PI = 3.1415926536D0
      TWOPI = 6.2831853072D0
      SIDER = 1.002737923D0
      CLIGHT = 2.997925D8
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 35
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, STBUFF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, STBUFF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XNAME2, NAME2)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      CLVER = IROUND (XGVER)
      BLVER = IROUND (XBVER)
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Find file, read CATBLK
      OLDCNO = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, STBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'WRIT', STBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
      DISKCL = DISKIN
      CNOCL = OLDCNO
      DISKSC = DISKIN
      CNOSC = OLDCNO
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FREQID = IROUND (XFQID)
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FREQID, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1050)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Load pointers for vis's.
      NRPARM = CATBLK(KIPCN)
      NAXIS  = CATBLK(KIDIM)
      NCMPLX = CATBLK(KINAX+JLOCC)
      NSTOKE = CATBLK(KINAX+JLOCS)
      NFREQS = CATBLK(KINAX+JLOCF)
      IF (NFREQS.LE.0) NFREQS = 1
      IF (NSTOKE.LE.0) NSTOKE = 1
      IF (NCOR.LE.0)   NCOR   = 1
      IF (NCOR.GT.2)   NCOR   = 2
C                                       Get observing bandwidth
      BANDW  = NFREQS * CATR(KRCIC+JLOCF)
C                                       Check sort order of input
      IF (ISORT(1:2).NE.'TB') THEN
         WRITE (MSGTXT,1060) ISORT
         JERR = 1
         GO TO 990
         END IF
C                                       Get source-observation info.
      CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
      CALL JULDAY (CHTM8, JD)
      CALL JD2DAT (JD, IT)
      CALL DAYYR  (IT(1), IT(2), IT(3), REFDAY)
      YEAR   = IT(1) + 1900
C                                       If the IAT corrections got lost,
C                                       calculate new ones.
      IF (ANTIAT.LE.0.0) THEN
         ANTUTC = 0.0
         ANTIAT = YEAR - 1973. + 12.
         END IF
C                                       Put new values in CATBLK.
      JERR = 0
C                                       Open text file (T sys, etc)
      CALL ZTXOPN ('READ', LUNK, FINDK, NAME2, F, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1070) IERR
         JERR = IERR
         GO TO 990
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANCLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1060 FORMAT ('INPUT VIS RECORDS MISORDERED, SORTED = ',A2,
     *        ' SHOULD BE = TB')
 1070 FORMAT ('ANCLIN: ERROR',I3,' OPENING TEXT FILE')
      END
      SUBROUTINE GCURVE (IERR)
C-----------------------------------------------------------------------
C   GCURVE is called from ANCAL. GCURVE reads the user's text
C   calibration file located in the AIPS run file subdirectory.
C   GCURVE calculates T ants from gain curves or lists of
C   measured T ants.
C     The CL  table is first sorted to antenna-time order and then
C   updated using the antenna-time sorted Tsys (TY) table from GETCAL
C   and Tant measurments in common.
C   Output in common:
C      NUMANT   I   The maximum antenna number
C      NUMPOL   I   Number (1 or 2) of polarizations/feeds
C      NUMIF    I   The number of IF in the data.
C   Output: IERR  I   return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INTEGER   IUTDAY, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NTERM, IPOL,
     *   NUMNOD, SOURID, ANTNO, LUNX, LUNG, I, JSRC, RELKOL, IMAKOL,
     *   RECI(13+32*MAXIF), IIF, IDX, IDAY, KKEY(2,2), KEYSUB(2,2),
     *   LSTSOU, ICLRNO,  LOOP, NUMREC, INPOL, POLOOP
      REAL      FKEY(2,2), DT, TANTUT, HARAD, ZADEG, HAHR, ELDEG, AZRAD,
     *   RELGAI, GNAMP, UTMIN, POLY, GMMOD, TSYST(2,MAXIF), THETA,
     *   ELRAD, RECORD(13+32*MAXIF)
      DOUBLE PRECISION PIBY2, RDTODG, TIME, ENDTIM, RECD(13+32*MAXIF)
      INCLUDE 'ANCAL.INC'
      INCLUDE 'ANCAL2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (RECORD, RECI, RECD)
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA   LUNG  /28/
      DATA LUNX /29/
C-----------------------------------------------------------------------
      PIBY2  = PI / 2.0D0
      RDTODG = 180.0D0 / PI
      ITYRNO = 0
      LSTSOU = -1
C                                       Read AN file.
      CALL GETANT (DISKCL, CNOCL, SUBA, CATBLK, SXBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Read the user's text cal
C                                        file. Load the T ant and gain
C                                        arrays, return start/stop
C                                        times for station T sys lists.
      CALL GETCAL (IERR)
      IF (IERR.NE.0.AND.IERR.NE.1) GO TO 999
      ITYRNO = 0
C                                       Do not update CL table?
      IF (CLVER.EQ.-1) GO TO 999
C                                       Reformat CL table?
      CALL CLREFM (DISKCL, CNOCL, CLVER, CATBLK, LUNG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                         Open CL table
      CALL CALINI ('READ', SXBUFF, DISKCL, CNOCL, CLVER, CATBLK, LUNG,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, NTERM,
     *   GMMOD, IERR)
      XGVER = CLVER
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Set sort keys
      KKEY(1,1) = CLIANT
      KKEY(2,1) = CLIANT
      KKEY(1,2) = CLDTIM
      KKEY(2,2) = CLDTIM
C                                       Set up polzn loop parms
      POLOOP = 1
      IF ((POLNO.EQ.3) .AND. (NUMPOL.EQ.2)) POLOOP = 2
      IF ((POLNO.EQ.3) .AND. (NUMPOL.EQ.1)) THEN
         IERR = 1
         MSGTXT = 'INDEX KEY SUGGESTS DUAL POLZN DATA'
         CALL MSGWRT (8)
         MSGTXT = 'CL TABLE CLAIMS ONLY 1 POLZN'
         GO TO 990
         END IF
C                                       Loop over polarizations
      DO 600 INPOL = 1, POLOOP
C                                       Real and imaginary column
C                                       pointers.
         IF (POLOOP.EQ.1) THEN
            RELKOL = CLKOLS(CLRRE1) - 1
            IMAKOL = CLKOLS(CLRIM1) - 1
            IPOL = 1
C                                       LCP in dual poln data?
            IF ((POLNO.EQ.2) .AND. (NUMPOL.GT.1)) THEN
               RELKOL = CLKOLS(CLRRE2) - 1
               IMAKOL = CLKOLS(CLRIM2) - 1
               IPOL = 2
               END IF
         ELSE IF (POLOOP.EQ.2) THEN
            IF (INPOL.EQ.1) THEN
               RELKOL = CLKOLS(CLRRE1) - 1
               IMAKOL = CLKOLS(CLRIM1) - 1
               IPOL = 1
            ELSE IF (INPOL.EQ.2) THEN
               RELKOL = CLKOLS(CLRRE2) - 1
               IMAKOL = CLKOLS(CLRIM2) - 1
               IPOL = 2
               END IF
            END IF
C                                       Load correct gain/offset
         DO 30 I = 1, MAXANT
            DPFU(I) = TDPFU(IPOL,I)
            GAOFFS(I) = TOFFS(IPOL,I)
 30         CONTINUE
C                                       Get number of records
         NUMREC = SXBUFF(5)
C                                       Close table
         CALL TABIO ('CLOS', 0, ICLRNO, SXBUFF, SXBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Sort to antenna time order.
         IF ((SXBUFF(43).NE.CLIANT) .OR. (SXBUFF(44).NE.CLDTIM)) THEN
            CALL TABSRT (DISKCL, CNOCL, 'CL', CLVER, CLVER, KKEY,
     *         KEYSUB, FKEY, SXBUFF, CATBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                         Reopen CL table read/write
         CALL CALINI ('WRIT', SXBUFF, DISKCL, CNOCL, CLVER, CATBLK,
     *      LUNG, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NUMNOD, NTERM, GMMOD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         ENDTIM = -1.0E10
C                                         Loop through time intervals,
C                                         calc and write gain curve
C                                         factors in CL file.
         DO 500 LOOP = 1,NUMREC
C                                       Read CL table
            ICLRNO = LOOP
            CALL TABIO ('READ', 0, ICLRNO, RECORD, SXBUFF, IERR)
            IF (IERR.LE.0) GO TO 200
            WRITE (MSGTXT,1030) IERR, 'READ'
            GO TO 990
C                                       Check subarray
 200        IF ((RECI(CLKOLS(CLISUB)).NE.SUBA) .AND.
     *         ((RECI(CLKOLS(CLISUB)).GT.0) .AND.
     *         (SUBA.GT.0))) GO TO 500
C                                       Check freq id
            IF ((RECI(CLKOLS(CLIFQI)).NE.FREQID) .AND. (FREQID.GT.0)
     *         .AND. (RECI(CLKOLS(CLIFQI)).GT.0)) GO TO 500
C                                       Get time, etc.
            TIME = RECD(CLKOLS(CLDTIM))
            DT = RECORD(CLKOLS(CLRTMI))
            SOURID = RECI(CLKOLS(CLISID))
            ANTNO = RECI(CLKOLS(CLIANT))
            UTMIN = TIME * 1440.0
            IUTDAY = TIME
            IDAY   = REFDAY + IUTDAY
C                                       Skip if no data read for
C                                       this antenna
            IF (NTSYS(ANTNO).EQ.0) GO TO 480
C                                       Get the Tsys
            CALL GETSYS (TIME, ANTNO, TSYST, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Find source info.
            IF (SOURID.EQ.LSTSOU) GO TO 260
            CALL GETSOU (SOURID, DISKCL, CNOCL, CATBLK, LUNX, IERR)
            IF (IERR.NE.0) GO TO 999
            LSTSOU = SOURID
C                                       Find source in KEYIN table
            DO 240 I = 1,MAXSRC
               JSRC = I
               IF (SNAME(1:8).EQ.SUNAM(I)(1:8)) GO TO 260
               IF (SUNAM(I).EQ.' ') THEN
                  SUNAM(I) = SNAME(1:8)
                  TFLUX(I) = 0.0
                  WRITE (MSGTXT,1230) SNAME
                  CALL MSGWRT (8)
                  GO TO 260
                  END IF
 240           CONTINUE
C                                       Source not in KEYIN file
            IERR = 10
            WRITE (MSGTXT,1240) SNAME
            GO TO 990
C                                       Loop thru stations and calc
C                                       gain curve values.
C                                       Interpolate antenna temps.
 260        IF (IGAIN(ANTNO).EQ.1) THEN
               CALL TWERP (ANTNO, TIME, TANTUT)
               IF (TFLUX(JSRC).GT.1.0E-10) TANTUT =
     *            TANTUT / TFLUX(JSRC)
               GO TO 450
               END IF
C                                      Calc gain curve
            CALL SOUELV (ANTNO, TIME, HARAD, ELRAD, AZRAD)
            ZADEG = 90.0 - ELRAD * RDTODG
            TANTUT= -1.0
            IF (HARAD.GT.PI) HARAD = HARAD - TWOPI
            IF (HARAD.LT.-PI) HARAD = HARAD + TWOPI
            HAHR  = HARAD * 12.0 / PI
C                                       Branch on computation type
            GO TO (450,300,320,340,360,380,490,490,490), IGAIN(ANTNO)
C
C                                      EQUAT gain curve.
 300        TANTUT = DPFU(ANTNO) * POLY (HAHR,  ANTNO, 6)
            GO TO 450
C                                      ALTAZ gain curve.
 320        TANTUT = DPFU(ANTNO) * POLY (ZADEG, ANTNO, 6)
            GO TO 450
C                                      NRAO 140' gain curve.
C                                      Updated Dec 1993. These are the
C                                      spherical harmonics in the 140'
C                                      VLB NUG calib. sheet. LJG.
C                                      Added 3 new terms in Dec 1993.
C                                      Order  of coefficients: Y00,
C                                      Y11o, Y10, Y11e, Y21o, Y20, Y21e,
C                                      Y22o, Y22e, Y30.
 340        THETA = PIBY2 - DECAPP
            TANTUT = DPFU(ANTNO)*(GACOEF(1,ANTNO)
     *         + GACOEF(2,ANTNO)*SIN(THETA)*SIN(HARAD)
     *         + GACOEF(3,ANTNO)*COS(THETA)
     *         + GACOEF(4,ANTNO)*SIN(THETA)*COS(HARAD)
     *         + GACOEF(5,ANTNO)*3.0*SIN(THETA)*COS(THETA)*SIN(HARAD)
     *         + GACOEF(6,ANTNO)*0.5*(3.0*COS(THETA)**2 - 1.0)
     *         + GACOEF(7,ANTNO)*3.0*SIN(THETA)*COS(THETA)*COS(HARAD))
     *         + GACOEF(8,ANTNO)*3.0*(SIN(THETA)**2 * SIN(2.0*HARAD))
     *         + GACOEF(9,ANTNO)*3.0*(SIN(THETA)**2 * COS(2.0*HARAD))
     *         + GACOEF(10,ANTNO)*(2.5*COS(THETA)**3 - 1.5*COS(THETA))
            GO TO 450
C                                      ELEV gain curve.
 360        ELDEG = 90.0 - ZADEG
            TANTUT = DPFU(ANTNO) * POLY (ELDEG,ANTNO,6)
            GO TO 450
C                                      ARECIBO gain curve.
C                                      From Harry Payne, 1983.
C                                      18 cm. only.
 380        RELGAI = 1.0 / (0.98372 + 1.0665E-2*(ZADEG-8.0)
     *         - 1.9939E-3*(ZADEG-8.0)**2 + 4.9621E-4*(ZADEG-8.0)**3)
            IF (ABS (ZADEG).LE.8.0) RELGAI = 1.0
            TANTUT = DPFU(ANTNO) * RELGAI
            GO TO 450
 450        CONTINUE
C                                       Correct Tant for antennas for
C                                       which Tant/Tsys given (e.g. VLA)
            IF (ISTATS(ANTNO)) TANTUT = TANTUT /
     *         MAX (1.0E-5, TFLUX(JSRC))
C                                       Update CL record
            DO 460 IIF = BIF, EIF
               IDX = IIF - BIF + 1
               GNAMP  = 0.0
               IF ((TANTUT.GT.0.0) .AND.
     *           (TSYST(IPOL,IDX).NE. FBLANK)) GNAMP  =
     *            SQRT (BDEF  * TSYST(IPOL,IDX) / TANTUT)
               IF (GNAMP.LT.1.0E-20) GNAMP = FBLANK
               IF ((GNAMP.EQ.FBLANK) .AND.
     *             ((RECORD(RELKOL+IIF).NE.FBLANK) .AND.
     *             (RECORD(RELKOL+IIF).GT.0.0))) GNAMP = 1.0
               IF (GNAMP.EQ.FBLANK) THEN
                  RECORD(RELKOL+IIF) = FBLANK
                  RECORD(IMAKOL+IIF) = FBLANK
               ELSE
                  IF ((RECORD(RELKOL+IIF).NE.FBLANK))
     *               RECORD(RELKOL+IIF) = RECORD(RELKOL+IIF) * GNAMP
                  IF ((RECORD(IMAKOL+IIF).NE.FBLANK))
     *               RECORD(IMAKOL+IIF) = RECORD(IMAKOL+IIF) * GNAMP
                  END IF
 460           CONTINUE
C                                        Write CL record
 480        CALL TABIO ('WRIT', 0, ICLRNO, RECORD, SXBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1490) IERR
               GO TO 990
               END IF
 490        CONTINUE
C
 500     CONTINUE
C                                       Close TY table
         CALL TABIO ('CLOS', 0, ITYRNO, STBUFF, SCBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         ITYRNO = 0
 600     CONTINUE
C                                       Close CL table
         CALL TABIO ('CLOS', 0, ICLRNO, STBUFF, SXBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('GCURVE: CLINI ERROR =',I3)
 1030 FORMAT ('GCURVE: TABIO ERROR =',I3,1X,A4,'ING CL TABLE')
 1230 FORMAT ('SOURCE ',A16,' NOT IN KEYIN INPUT. ADDED WITH FLUX 0.0')
 1240 FORMAT ('SOURCE ',A16,' NOT IN KEYIN CALIBRATION TABLE')
 1490 FORMAT ('GCURVE: ERROR =',I3,' FROM EXTIO')
      END
      SUBROUTINE GETCAL (IERR)
C-----------------------------------------------------------------------
C   GETCAL reads the external text file containing T sys, T ant,
C   gains, fluxes, etc.  Tsys measurments are written into an TY table
C   and sorted into antenna time order.
C   Output in common:
C      BIF        I    First IF number from file
C      EIF        I    Last IF number from file
C      POLNO      I    Polarization number from file (mode=1 only)
C   Output:
C      IERR       I    Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTYTAB.INC'
      INTEGER NPRM
      PARAMETER (NPRM = 168)
C
      CHARACTER PARS(NPRM)*8, ENDMRK*8, CHFLUX*8, CHSOUR*8,
     *   CHORDR*8, CDUMMY*80, VALCH(NPRM)*8
      INTEGER   KMODE, NPARS, NVALS, NDUM, ITSYS, IVERS, LASTDY, NDY,
     *   IDIV, TYKOLS(MAXTYC), TYNUMV(MAXTYC), LASTST, I, IA, IB, ITYP,
     *   ISTN, KSTN1, KSTN, ISTN2, IDUM, ITANT, K, JSRC, MXANT,
     *   KKEY(2,2), KEYSUB(2,2), JSTN, NIF, SID, SUB, FQID, IPOL, CHANB,
     *   CHANE, TORDER(MAXIF), KPOL, KIF, INCHB, INCHE, IFJMP, IPTR,
     *   INBIF, INEIF, NCOL
      LOGICAL   GOTCTL, ALLCHN, BOTHSI, INDEX, POLSET, EIFSET
      REAL      FT, TIMOFF, FKEY(2,2), TTIME, TSYST(2,MAXIF),
     *   TANT(2,MAXIF), XTI
      DOUBLE PRECISION VALS(NPRM), DEFS(NPRM)
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA KKEY /TYIANT, TYIANT, TYRTIM, TYRTIM/
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA PARS/'BASELINE', 'TSYS    ', 'TANT    ',
     *   'GAIN    ', 'CONTROL ', 'UNBIAS  ', 'CALERR  ',
     *   'BDEF????', 'B       ', 'FT      ', 30*'********',
     *   60*'        ',
     *   'DPFU    ', 'DPFU    ', 'OFFSET  ', 'OFFSET  ',
     *   'POLY    ', 'POLY    ', 'POLY    ', 'POLY    ',
     *   'POLY    ', 'POLY    ', 'EQUAT   ', 'ALTAZ   ',
     *   'GCNRAO  ', 'EL???   ', 'GCAREC??', 'HISTORY ',
     *   'HISTORY ', 'HISTORY ', 'HISTORY ', 'HISTORY ',
     *   'HISTORY ', 'HISTORY ', 'SRC/SYS ', 'IFNO    ',
     *   'POLNO   ', 'TIMEOFF ', 'A00     ', 'A11O    ',
     *   'A10     ', 'A11E    ', 'A21O    ', 'A20     ',
     *   'A21E    ', 'A22O    ', 'A22E    ', 'A30     ',
     *   'BIF     ', 'EIF     ', 'ALLCHAN ',
     *   'BOTHSIDE', 28*'        '/
      DATA CHFLUX,     CHSOUR,    CHORDR,    ENDMRK
     *   /'FLUX??  ', 'SOURCE? ','INDEX?  ','/       '/
      DATA DEFS/7*-1.0D0,1.0D0,2*-1.0D0,30*-1.0D0,
     *   30*0.0D0,30*-1.0D0,-1.0D0,-1.0D0,0.0D0,0.0D0,
     *   1.0D0,5*0.0D0,12*-1.0D0,-1.0D0,-1.0D0,-1.0D0,0.0D0,
     *   10*0.0D0, 4*-1.0D0, 28*-1.D0/
      DATA NDUM /10000/
      DATA LASTST /0/
      DATA SID, SUB, FQID /-1, -1, -1/
C-----------------------------------------------------------------------
      NPARS = NPRM
      DO 20 I = 1,MAXANT
         ISTATS(I) = .FALSE.
 20      CONTINUE
      CALL FILL (20, 0, NDAYS)
      CALL RFILL (2*MAXIF, FBLANK, TANT)
      CALL RFILL (2*MAXIF, FBLANK, TSYST)
      MXANT = MAXANT
      IPOL = 1
      BIF = 1
      EIF = 1
      POLNO = 1
      POLSET = .FALSE.
      EIFSET = .FALSE.
      ALLCHN = .FALSE.
      BOTHSI = .FALSE.
      INDEX = .FALSE.
C                                      Open Tsys table.
      ITYVER = 0
      LUNSC = 30
      NIF = 1
      IF (JLOCIF.GT.0) NIF = CATBLK(KINAX+JLOCIF)
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      CALL TYINI ('WRIT', SXBUFF, DISKSC, CNOSC, ITYVER, CATBLK, LUNSC,
     *   ITYRNO, TYKOLS, TYNUMV, NUMPOL, NIF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Prepare to read KEYIN file.
C                                      Load station names in STNNAM
C                                      from AN file into KEYIN parms
C                                      array PARS.
      DO 50 I = 1, NSTNS
         PARS(10+I) = STNNAM(I)
 50      CONTINUE
C                                       Init antenna counts, bl factors
      DOBFAC = .FALSE.
      GOTCTL = .FALSE.
      DO 60 IA = 1,MXANT
         IGAIN(IA) = 0
         NTANT(IA) = 0
         NTSYS(IA) = 0
         DO 60 IB = 1,MXANT
            BFAC(IA,IB) = 1.0
 60         CONTINUE
      DO 80 I = 1,30
         PARS(I+40) = CHSOUR
         PARS(I+70) = CHFLUX
 80      CONTINUE
      DO 90 I = 1,28
         PARS(I+140) = CHORDR
 90      CONTINUE
C
C                                      Read a complete text entry up to
C                                      the endmark '/'. Load defaults
C                                      into the KEYIN output buffer
C                                      first.
 100  KMODE = 0
      DO 140 I = 1,NPARS
         VALS(I) = DEFS(I)
         VALCH(I) = '        '
 140     CONTINUE
      CALL KEYIN (PARS, VALS, VALCH, NPARS, ENDMRK, KMODE, LUNK, FINDK,
     *   IERR)
      IF (IERR.NE.0) THEN
C                                      Returns IERR=1 on EOF.
         IF (IERR.EQ.1) GO TO 700
         WRITE (MSGTXT,1140) IERR
         GO TO 990
         END IF
C
C                                      Which type of group has just been
C                                      read ?
      DO 160 I = 1,5
         ITYP  = I
         IF (VALS(I).NE.-1.D0) GO TO 170
 160     CONTINUE
C                                      Which station name has KEYIN
C                                      just read ?
 170  DO 180 I = 1, NSTNS
         ISTN = I
         JSTN = I
         IF (VALS(I+10).NE.-1.0D0) GO TO 190
 180     CONTINUE
      ITYP = 5
C                                       Only allow 1 "CONTROL" card
C                                       (blank entries default to
C                                       "CONTROL")
      IF ((ITYP.EQ.5) .AND. GOTCTL) GO TO 100
C
 190   CONTINUE
C                                       Get BIF, EIF and POLNO
      IF (VALS(124).GT.0.0D0) THEN
         BIF = NINT (VALS(124))
         EIF = BIF
         END IF
      IF (VALS(125).GT.0.0D0) THEN
         POLNO = NINT (VALS(125))
         POLSET = .TRUE.
         END IF
      IF (VALS(137).GT.0.0D0) BIF = NINT (VALS(137))
      IF (VALS(138).GT.0.0D0) THEN
         EIF = NINT (VALS(138))
         EIFSET = .TRUE.
         END IF
      BIF = MAX (1, BIF)
      EIF = MAX (BIF, EIF)
      IF ((POLNO.EQ.2) .AND. (NUMPOL.GT.1)) IPOL = 2
C                                       Jump to appropriate group.
      GO TO (200,300,400,500,600) ITYP
C-----------------------------------------------------------------------
C                                      BASELINE cal card was read.
 200  CONTINUE
      KSTN1 = ISTN + 1
      DO 210 KSTN = KSTN1, NSTNS
         ISTN2    = KSTN
         IF (VALS(KSTN+10).NE.-1D0) GO TO 220
 210     CONTINUE
C                                       Couldn't find 2nd antenna
      WRITE (MSGTXT,1210)
      IERR = 1
      GO TO 990
 220  CONTINUE
      IA = MIN (ISTN, ISTN2)
      IB = MAX (ISTN, ISTN2)
      BFAC(IA,IB) = VALS(9)
      DOBFAC = .TRUE.
      GO TO 100
C
C                                      TSYS cal card was read.
C                                      Read T sys list.
 300  CONTINUE
      KMODE = 3
      NVALS = NDUM
      TIMOFF = VALS(126) / 86400.0D0
      CALL KEYIN (CDUMMY, DUMMY, VALCH, NVALS, ENDMRK, KMODE, LUNK,
     *   FINDK, IERR)
      IF (IERR.EQ.0) GO TO 310
         WRITE (MSGTXT,1300) IERR
         GO TO 990
C                                      Load T sys etc. in TY table
 310  FT = VALS(10)
      IF (FT.LE.0.0) FT = 1.0
      LASTDY= 0
      NDY   = 0
      IDIV  = 0
      ITSYS = 0
C                                       Check if index card read
      IF (VALCH(141).NE.'        ') THEN
         CALL CHKORD (VALCH(141), TORDER, INCHB, INCHE, INBIF,
     *      INEIF, POLNO, NCOL)
C                                       Some simple checks
         IF (POLNO.EQ.3) THEN
            IF (NCOR.EQ.1) THEN
               IERR = 1
               WRITE (MSGTXT,1000) STNNAM(JSTN)
               CALL MSGWRT (6)
               MSGTXT = 'NO. POLZN IN INDEX KEYWORD DOES NOT MATCH DATA'
               GO TO 990
               END IF
            END IF
         IF (POLNO.EQ.1) THEN
            IF (ICOR0.NE.-1) THEN
               IERR = 1
               WRITE (MSGTXT,1000) STNNAM(JSTN)
               CALL MSGWRT (6)
               MSGTXT = 'RHC POLZN IN INDEX KEYWORD DOES NOT MATCH DATA'
               GO TO 990
               END IF
            END IF
         IF (POLNO.EQ.2) THEN
            IF ((NCOR.EQ.1) .AND. (ICOR0.NE.-2)) THEN
               IERR = 1
               WRITE (MSGTXT,1000) STNNAM(JSTN)
               CALL MSGWRT (6)
               MSGTXT = 'LHC POLZN IN INDEX KEYWORD DOES NOT MATCH DATA'
               GO TO 990
               END IF
            END IF
         INDEX = .TRUE.
         END IF
C                                       IF range
      CHANB = BIF
      CHANE = EIF
      IF (INDEX) THEN
         CHANB = INCHB
         CHANE = INCHE
         BIF = INBIF
         EIF = INEIF
         END IF
      IF (JLOCIF.GT.0) EIF = MIN (EIF, CATBLK(KINAX+JLOCIF))
C                                       One number for all channels
      ALLCHN = VALS(139).NE.-1.D0
      IF (ALLCHN) THEN
         IF (INDEX) THEN
            IERR = 2
            WRITE (MSGTXT,1000) STNNAM(JSTN)
            CALL MSGWRT (6)
            MSGTXT = 'ALLCHAN & INDEX SET - INVALID CONSTRUCT'
            GO TO 990
            END IF
         CHANB = 1
         CHANE = 1
         IF ((.NOT.POLSET) .AND. (NCOR.GT.1)) POLNO = 3
         IF ((.NOT.EIFSET) .AND. (JLOCIF.GT.0)) THEN
            EIF = NIF
            END IF
         END IF
C                                       Upper & lower sidebands stored
C                                       as different IF's, but only
C                                       have one Tsys for them.
      BOTHSI = VALS(140).NE.-1.D0
      IF (ALLCHN .AND. BOTHSI) THEN
         IERR = 1
         WRITE (MSGTXT,1000) STNNAM(JSTN)
         CALL MSGWRT (6)
         MSGTXT = 'ALLCHAN & BOTHSIDE SET - INVALID CONSTRUCT'
         GO TO 990
         END IF
      IFJMP = 1
      IF (BOTHSI) THEN
         IFJMP = 2
         CHANE = CHANE / 2
         END IF
C                                       Tant/Tsys given?
      IF (VALS(123).NE.-1.0D0) IDIV = 1
      ISTATS(JSTN) = ABS (VALS(123)+1.0D0) .GT. 1.0D-5
      IF (.NOT.INDEX) THEN
         DO 330 IDUM = 1,NVALS,3+CHANE-CHANB
            CALL CHKCRD (DUMMY(IDUM), JSTN, 1, IERR)
            IF (IERR.NE.0) GO TO 999
            TTIME = (DUMMY(IDUM) - REFDAY) + DUMMY(IDUM+1) /
     *         24.0 + TIMOFF
            DO 320 I = 1,NIF,IFJMP
               IF (ALLCHN) THEN
C                                       One number for all channels
C                                       1 polzn
                  IF (DUMMY(IDUM+2).LE.0.0) THEN
C                                       Negative Tsys
                     TSYST(IPOL,I) = FBLANK
                     IF ((POLNO.EQ.3).AND.(IPOL.EQ.1))
     *                  TSYST(IPOL+1,I) = FBLANK
                     WRITE (MSGTXT,1320) DUMMY(IDUM+2), STNNAM(JSTN)
                     CALL MSGWRT (8)
                  ELSE
                     TSYST(IPOL,I) = DUMMY(IDUM+2) * FT
                     IF (IDIV.EQ.1) TSYST(IPOL,I) = 1.0 / TSYST(IPOL,I)
C                                       2nd polzn
                     IF ((POLNO.EQ.3) .AND. (IPOL.EQ.1)) THEN
                        TSYST(IPOL+1,I) = DUMMY(IDUM+2) * FT
                        IF (IDIV.EQ.1) TSYST(IPOL+1,I) =
     *                     1.0 / TSYST(IPOL+1,I)
                        END IF
                     END IF
C
               ELSE IF (BOTHSI) THEN
C                                       One number for both sidebands
                  IF (DUMMY(IDUM+2).LE.0.0) THEN
                     TSYST(IPOL,I) = FBLANK
                     TSYST(IPOL,I+1) = FBLANK
                     WRITE (MSGTXT,1320) DUMMY(IDUM+2), STNNAM(JSTN)
                     CALL MSGWRT (8)
                  ELSE
                     TSYST(IPOL,I) = DUMMY(IDUM+2) * FT
                     TSYST(IPOL,I+1) = DUMMY(IDUM+2) * FT
                     IF (IDIV.EQ.1) TSYST(IPOL,I) = 1.0 / TSYST(IPOL,I)
                     IF (IDIV.EQ.1) TSYST(IPOL,I+1) = 1.0 /
     *                  TSYST(IPOL,I+1)
                     END IF
C
               ELSE
C                                       One to one correspondance
                  IF (DUMMY(IDUM+I+1).LE.0.0) THEN
                     TSYST(IPOL,I) = FBLANK
                     WRITE (MSGTXT,1320) DUMMY(IDUM+I+1), STNNAM(JSTN)
                     CALL MSGWRT(8)
                  ELSE
                     TSYST(IPOL,I) = DUMMY(IDUM+I+1) * FT
                     IF (IDIV.EQ.1) TSYST(IPOL,I) = 1.0 / TSYST(IPOL,I)
                     END IF
                  END IF
 320           CONTINUE
C
            CALL TABTY ('WRIT', SXBUFF, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *            NIF, TTIME, XTI, SID, JSTN, SUB, FQID, TSYST, TANT,
     *            IERR)
            IF (IERR.NE.0) GO TO 999
            ITSYS = ITSYS + 1
 330        CONTINUE
C                                       Re-order columns
      ELSE IF (INDEX) THEN
         DO 350 IDUM = 1,NVALS,2+NCOL
            CALL CHKCRD (DUMMY(IDUM), JSTN, 1, IERR)
            IF (IERR.NE.0) GO TO 999
            TTIME = (DUMMY(IDUM) - REFDAY) + DUMMY(IDUM+1) /
     *         24.0 + TIMOFF
            IPTR = 0
            DO 340 I = 1,NCOL,1
               IPTR = IPTR + 1
               KPOL = 1
               IF (TORDER(IPTR).GT.100) KPOL = 2
               KIF = TORDER(IPTR) - 100*(KPOL-1)
               IF (KIF.GT.0) THEN
                  IF (BOTHSI) THEN
                     IF (DUMMY(IDUM+IPTR+1).LE.0.0) THEN
C                                       Negative system temperature
                        TSYST(KPOL,KIF) = FBLANK
                        TSYST(KPOL,KIF+1) = FBLANK
                        WRITE (MSGTXT,1320) DUMMY(IDUM+IPTR+1),
     *                     STNNAM(JSTN)
                        CALL MSGWRT (8)
                     ELSE
                        TSYST(KPOL,KIF) = DUMMY(IDUM+IPTR+1) * FT
                        TSYST(KPOL,KIF+1) = DUMMY(IDUM+IPTR+1) * FT
                        IF (IDIV.EQ.1) TSYST(KPOL,KIF) =
     *                     1.0 / TSYST(KPOL,KIF)
                        IF (IDIV.EQ.1) TSYST(KPOL,KIF+1) =
     *                     1.0 / TSYST(KPOL,KIF+1)
                        END IF
                     ELSE IF (.NOT.BOTHSI) THEN
                        IF (DUMMY(IDUM+IPTR+1).LE.0.0) THEN
C                                       Negative system temperature
                           TSYST(KPOL,KIF) = FBLANK
                           WRITE (MSGTXT,1320) DUMMY(IDUM+IPTR+1),
     *                        STNNAM(JSTN)
                           CALL MSGWRT (8)
                        ELSE
                           TSYST(KPOL,KIF) = DUMMY(IDUM+IPTR+1) * FT
                           IF (IDIV.EQ.1) TSYST(KPOL,KIF) =
     *                        1.0 / TSYST(KPOL,KIF)
                           END IF
                        END IF
                  END IF
 340           CONTINUE
            CALL TABTY ('WRIT', SXBUFF, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *            NIF, TTIME, XTI, SID, JSTN, SUB, FQID, TSYST, TANT,
     *            IERR)
            IF (IERR.NE.0) GO TO 999
            ITSYS = ITSYS + 1
 350        CONTINUE
         END IF
      WRITE (MSGTXT,1330) ITSYS, STNNAM(JSTN)
      CALL MSGWRT (8)
      NTSYS(JSTN) = ITSYS
      LASTST = JSTN
      ALLCHN = .FALSE.
      GO TO 100
C
C                                      TANT cal card was read.
C                                      Read T ant list and load
C                                      into TANT array.
 400  CONTINUE
      FT = VALS(10)
      TIMOFF = VALS(126) / 86400.0D0
      IF (FT.LE.0.0) FT = 1.0
      KMODE = 3
      NVALS= NDUM
      CALL KEYIN (CDUMMY, DUMMY, VALCH, NVALS, ENDMRK, KMODE, LUNK,
     *   FINDK, IERR)
      IF (NVALS.GT.3*MXTANT) THEN
         WRITE (MSGTXT,1420) MXTANT, STNNAM(ISTN)
         IERR = 1
         GO TO 990
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1440) IERR
         GO TO 990
         END IF
C
      ITANT = 0
      DO 460 IDUM = 1, NVALS, 3
         ITANT    = ITANT + 1
         CALL CHKCRD (DUMMY(IDUM), JSTN, 2, IERR)
         TANTS(1,ISTN,ITANT) = (DUMMY(IDUM) - REFDAY)
     *                       +   DUMMY(IDUM+1) / 24.0 + TIMOFF
         TANTS(2,ISTN,ITANT) =   DUMMY(IDUM+2) * FT
 460     CONTINUE
      NTANT(ISTN) = ITANT
      IGAIN(ISTN) = 1
      GO TO 100
C
C                                      GAIN cal card was read.
C                                      Load gain curve coefficents
C                                      into GACOEF and GAOFFS.
 500  TDPFU(1,ISTN) = VALS(101)
      TDPFU(2,ISTN) = VALS(102)
      IF ((TDPFU(1,ISTN).LE.0.0) .AND. (TDPFU(2,ISTN).GT.0.0)) THEN
         TDPFU(1,ISTN) = TDPFU(2,ISTN)
         END IF
      IF ((TDPFU(2,ISTN).LE.0.0) .AND. (TDPFU(1,ISTN).GT.0.0)) THEN
         TDPFU(2,ISTN) = TDPFU(1,ISTN)
         END IF
      TOFFS(1,ISTN) = VALS(103)
      TOFFS(2,ISTN) = VALS(104)
      IF ((TOFFS(1,ISTN).LE.0.0) .AND. (TOFFS(2,ISTN).GT.0.0)) THEN
         TOFFS(1,ISTN) = TOFFS(2,ISTN)
         END IF
      IF ((TOFFS(2,ISTN).LE.0.0) .AND. (TOFFS(1,ISTN).GT.0.0)) THEN
         TOFFS(2,ISTN) = TOFFS(1,ISTN)
         END IF
CCCC      DPFU(ISTN) = VALS(101)
CCCC      GAOFFS(ISTN) = VALS(103)
      DO 510 K = 1,6
         GACOEF(K,ISTN) = VALS(104+K)
 510     CONTINUE
C
C                                       Save antenna solution type.
      IF (VALS(111).NE.-1D0) IGAIN(ISTN) = 2
      IF (VALS(112).NE.-1D0) IGAIN(ISTN) = 3
      IF (VALS(113).NE.-1D0) IGAIN(ISTN) = 4
      IF (VALS(114).NE.-1D0) IGAIN(ISTN) = 5
      IF (VALS(115).NE.-1D0) IGAIN(ISTN) = 6
C
C                                       Load the NRAO 140' coefficients
      IF (VALS(113).NE.-1D0) THEN
         DO 515 K = 1, 10
            GACOEF(K,ISTN) = VALS(126+K)
 515        CONTINUE
         END IF
C
      GO TO 100
C
C                                     Load source flux, B default
C                                     here.
 600  BDEF = VALS(8)
      GOTCTL = .TRUE.
C                                     Set SUNAM's to blank and TFLUX's
C                                     to 1.0 for defaults.  Do for
C                                     all MAXSRC elements.
      DO 605 JSRC = 1,MAXSRC
         SUNAM(JSRC) = '        '
         TFLUX(JSRC) = 0.0
 605     CONTINUE
C                                     Now fill up to 30 sources from
C                                     input data.
      DO 610 JSRC = 1,30
         SUNAM(JSRC) = VALCH(40+JSRC)
         IF( SUNAM(JSRC) .NE. ' ' ) TFLUX(JSRC) = VALS(70+JSRC)
         WRITE (MSGTXT,1610) SUNAM(JSRC), TFLUX(JSRC)
         IF (TFLUX(JSRC).GT.0.0) CALL MSGWRT (8)
 610     CONTINUE
      GO TO 100
C                                       Close up
 700  CALL TABIO ('CLOS', 0, ITYRNO, SXBUFF, SXBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTXCLS (LUNK, FINDK, IERR)
C                                       Normalize baseline corrections
C                                       wrst default value.
      DO 710 IA = 1, NSTNS
C                                       Check the Tsys found for all
         IF (NTSYS(IA).EQ.0) THEN
            WRITE (MSGTXT,1710) STNNAM(IA)
            CALL MSGWRT (8)
            END IF
         DO 710 IB = 1, NSTNS
            IF (BFAC(IA,IB).NE.1.0.AND.BFAC(IA,IB).NE.0.0)
     *          BFAC(IA,IB) = BFAC(IA,IB) / BDEF
 710        CONTINUE
C                                       Sort TY table
      CALL TABSRT (DISKSC, CNOSC, 'TY', IVERS, IVERS, KKEY, KEYSUB,
     *   FKEY, SXBUFF, CATBLK, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETCAL: FOR STATION ',A)
 1140 FORMAT ('GETCAL: ERROR =',I3,' FROM FIRST KEYIN CALL')
 1210 FORMAT ('GETCAL: SECOND STATION NOT FOUND ON BASELINE CARD')
 1300 FORMAT ('GETCAL: ERROR =',I3,' FROM KEYIN SKIPPING TSYS CARDS')
 1320 FORMAT ('GETCAL: TSYS ',F8.1,' <0 BLANKED FOR STATION: ',A8)
 1330 FORMAT (I5,' System temps read for station : ',A)
 1420 FORMAT ('MAX. OF I4 ANT. TEMPS EXCEEDED FOR STN :',A)
 1440 FORMAT ('GETCAL: ERROR =',I3,' FROM KEYIN READING TANT CARDS')
 1610 FORMAT ('Source : ',A8,' Flux density (Jy) = ',F8.3)
 1710 FORMAT ('NO SYSTEM TEMPS FOUND FOR STATION : ',A)
      END
      SUBROUTINE CHKCRD (TVALS, ISTN, ITYP, IERR)
C-----------------------------------------------------------------------
C   Checks T sys or T ant cards read by KEYIN in GETCAL.
C   Inputs:
C    ISTN       I    Antenna number
C    ITYP       I    Temp. type, 1=Tsys, 2=Tant
C   Input/Output:
C    TVALS(3:)  D    On input KEYIN values
C   Output:
C    IERR       I    Error code, 0=>OK, 10 = bad entry.
C-----------------------------------------------------------------------
      CHARACTER TCARD(2)*4
      INTEGER    IERR, ITYP, ISTN, LTYP, LSTN
      INTEGER    IDYNUM, IHR, IMIN
      REAL       LTIME, TIME
      DOUBLE PRECISION TVALS(*)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA TCARD /'TSYS','TANT'/
      DATA LTIME, LSTN, LTYP /0.0,0,0/
C-----------------------------------------------------------------------
      IERR = 0
      TIME = TVALS(1) * 24.0 + TVALS(2)
      IF ((TIME.LE.LTIME) .AND. (ISTN.EQ.LSTN) .AND.
     *   (ITYP.EQ.LTYP)) GO TO 30
      IF (TVALS(1).GE.1.0.AND.TVALS(1).LE.365.0.AND.
     *    TVALS(2).GE.0.0.AND.TVALS(2).LE.24.0.AND.
     *    TVALS(3).GT.0.0) GO TO 100
 30       IDYNUM = TVALS(1) + 0.01
          IHR    = TVALS(2) + 0.01
          IMIN   = (TVALS(2) - IHR ) * 60.0 + 0.01
          IF (IDYNUM.GT.9999) IDYNUM = 99999
          IF (IDYNUM.LT.-9999) IDYNUM = -99999
          IF (IHR.GT.99) IHR = 99
          IF (IHR.LT.-9) IHR = -9
          IF (IMIN.GT.99) IMIN = 99
          IF (IMIN.LT.-9) IMIN = -9
          WRITE (MSGTXT,1100) STNNAM(ISTN), TCARD(ITYP), IDYNUM, IHR,
     *      IMIN, TVALS(3)
          CALL MSGWRT (8)
          IERR = 10
 100  LTIME = TIME
      LSTN  = ISTN
      LTYP  = ITYP
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('BAD ENTRY IN ',A,1X,A,' CARD :',I5,I3,':',I2,F8.1)
      END
      SUBROUTINE TWERP (ISTN, UT, TOUT)
C-----------------------------------------------------------------------
C   Interpolate T ants at time UT from table in common.
C    Inputs:
C      ISTN    I    Antenna number
C      UT      D    Time (days)
C   Output:
C      TOUT    R    The Tant.
C   J.M. Benson  1 Nov 1983
C-----------------------------------------------------------------------
      INTEGER    ISTN
      DOUBLE PRECISION UT
      REAL       TOUT
C
      INTEGER   LO, HI, NTA, I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      NTA = NTANT(ISTN)
C                                       Interpolate antenna temp
      IF (UT.LE.TANTS(1,ISTN,1)) THEN
         TOUT = TANTS(2,ISTN,1)
         GO TO 999
         END IF
      IF (UT.GT.TANTS(1,ISTN,NTA)) THEN
         TOUT = TANTS(2,ISTN,NTA)
         GO TO 999
         END IF
C
      DO 210 I = 1, NTA - 1
         LO    = I
         HI    = I + 1
         IF (UT.GE.TANTS(1,ISTN,LO).AND.
     *       UT.LE.TANTS(1,ISTN,HI))  GO TO 220
 210     CONTINUE
C
 220  TOUT = TANTS(2,ISTN,LO) + (TANTS(2,ISTN,HI)-TANTS(2,ISTN,LO))*
     *                          (            UT-TANTS(1,ISTN,LO))/
     *                          (TANTS(1,ISTN,HI)-TANTS(1,ISTN,LO))
      GO TO 999
C
 999  RETURN
      END
      FUNCTION POLY (X, ISTN, NTERM)
C-----------------------------------------------------------------------
C   Evaluate the antenna gain polynomial for antenna ISTN at position
C   X using NTERM terms.
C   Input:
C    X      R    Argument of polynimial
C    ISTN   I    Antenna number
C    NTERM  I    Number of terms to use
C-----------------------------------------------------------------------
      INTEGER    NTERM, ISTN, I
      REAL       ARG, X, XARG, POLY
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      POLY = 0.0
      IF (NTERM.LT.1) GO TO 999
      IF (NTERM.GT.6) GO TO 999
      POLY = GACOEF(1,ISTN)
      IF (NTERM.LT.2) GO TO 999
      XARG = X + GAOFFS(ISTN)
      ARG  = 1.0
      DO 10 I = 2, NTERM
         ARG  = ARG * XARG
 10      POLY = POLY + GACOEF(I,ISTN) * ARG
C
 999  RETURN
      END
      SUBROUTINE DAYYR (YR, MN, DY, DAY)
C-----------------------------------------------------------------------
C   Calculate day number from year, month, day of month.
C   Inputs:
C    YR     I    Year number
C    MN     I    Month number
C    DY     I    Day of month
C   Output:
C    DAY    I    Day of year number
C   J.M. Benson, 1 Nov 1983
C-----------------------------------------------------------------------
      INTEGER   YR, MN, DY, DAY, DAYSUM(12)
      DATA DAYSUM /0,31,59,90,120,151,181,212,243,273,304,334/
C-----------------------------------------------------------------------
      DAY = DAYSUM (MN) + DY
      IF ((MOD(YR,4).EQ.0).AND.(MOD(YR,100).NE.0).AND.MN.GT.2)
     *   DAY = DAY + 1
C
 999  RETURN
      END
      SUBROUTINE BLFAC (IERR)
C-----------------------------------------------------------------------
C   BLFAC writes a BL table using values in BFAC.
C   Input in common:
C      BFAC     R(*,*)  Baseline amplitude factors.
C      NUMANT   I       The maximum antenna number
C      NUMPOL   I       Number (1 or 2) of polarizations/feeds
C      NUMIF    I       The number of IF in the data.
C   Input/Output in common:
C       BLVER   I       Output BL table number
C   Output:
C      IERR     I       Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IBLRNO, BLKOLS(MAXBLC), BLNUMV(MAXBLC), LOOPA1, LOOPA2,
     *   LOOPIF,  BLSID, BLSUB, BLANT1, BLANT2, BLFQID, LUN
      REAL      BLTIME, FACMUL(2,2,MAXIF), FACADD(2,2,MAXIF)
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open/create BL table.
      LUN = 29
      CALL BLINI ('WRIT', SCBUFF, DISKCL, CNOCL, BLVER, CATBLK, LUN,
     *   IBLRNO, BLKOLS, BLNUMV, NUMANT, NUMPOL, NUMIF, IERR)
      IF (IERR.NE.0) GO TO 999
      IBLRNO = 1
C                                       Set table wide values.
      BLTIME = 1.0
      BLSID = -1
      BLSUB = SUBA
      BLFQID = FREQID
C                                       Zero additive errors, imaginary
C                                       parts of mutiplicative errors.
      DO 50 LOOPIF = 1,NUMIF
         FACADD(1,1,LOOPIF) = 0.0
         FACADD(2,1,LOOPIF) = 0.0
         FACADD(1,2,LOOPIF) = 0.0
         FACADD(2,2,LOOPIF) = 0.0
         FACMUL(1,1,LOOPIF) = 1.0
         FACMUL(2,1,LOOPIF) = 0.0
         FACMUL(1,2,LOOPIF) = 1.0
         FACMUL(2,2,LOOPIF) = 0.0
 50      CONTINUE
C                                       Loop over first antenna
      DO 200 LOOPA1 = 1,NUMANT
         DO 100 LOOPA2 = LOOPA1+1,NUMANT
C                                       Set baseline values
            BLANT1 = LOOPA1
            BLANT2 = LOOPA2
            DO 60 LOOPIF = 1,NUMIF
               FACMUL(1,1,LOOPIF) = BFAC(BLANT1,BLANT2)
               FACMUL(1,2,LOOPIF) = BFAC(BLANT1,BLANT2)
 60            CONTINUE
C                                       Write table entry
            CALL TABBL ('WRIT', SCBUFF, IBLRNO, BLKOLS, BLNUMV, NUMPOL,
     *         BLTIME, BLSID, BLSUB, BLANT1, BLANT2, BLFQID, FACMUL,
     *         FACADD, IERR)
            IF (IERR.NE.0) GO TO 999
 100        CONTINUE
 200     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, IBLRNO, SCBUFF, SCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (7)
         END IF
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('BLFAC: ERROR ',I5, ' CLOSING BL TABLE')
      END
      SUBROUTINE ANCLHI
C-----------------------------------------------------------------------
C   ANCLHI copies and updates history file.
C-----------------------------------------------------------------------
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3)
      CHARACTER HILINE*72, LABEL*8, CTIME*20
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ANCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), STBUFF, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(13:20), CTIME(1:12))
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME(1:12), CTIME(13:20)
      CALL HIADD (LUN1, HILINE, STBUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       File name
      WRITE (HILINE,2000) TSKNAM, NAME2
      CALL HIADD (LUN1, HILINE, STBUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF and Poln. no.
      WRITE (HILINE,2001) TSKNAM, BIF, EIF, POLNO
      CALL HIADD (LUN1, HILINE, STBUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SUBARRAY, GAINVER
      WRITE (HILINE,2002) TSKNAM, SUBA, CLVER
      CALL HIADD (LUN1, HILINE, STBUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       BLVER
      WRITE (HILINE,2003) TSKNAM, BLVER
      CALL HIADD (LUN1, HILINE, STBUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add any user supplied history.
         IF (NUMHIS.GT.0) THEN
            WRITE (LABEL,1011) TSKNAM
            DO 15 I = 1,NUMHIS
               HILINE = LABEL // HISCRD(I)
               CALL HIADD (LUN1, HILINE, STBUFF, IERR)
               IF (IERR.NE.0) GO TO 100
 15            CONTINUE
            END IF
C                                       Close HI file
 100  CALL HICLOS (LUN1, .TRUE., STBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANCLHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A,' ''  /********* Start ',
     *   A12,2X,A8)
 1011 FORMAT (A6,' /')
 2000 FORMAT (A6, ' IN2NAME = ''',A48,'''')
 2001 FORMAT (A6, ' / BIF =',I3,' EIF =',I3,' Polno =',I3,
     *   ' from text file')
 2002 FORMAT (A6, ' SUBARRAY =',I3,' GAINVER = ',I4,' /CL table')
 2003 FORMAT (A6, ' BLVER = ',I4,' /BL table')
      END
      SUBROUTINE GETSYS (TIME, ANTNO, TSYST, IERR)
C-----------------------------------------------------------------------
C   Interpolate Tsys for Antenna no. ANTNO at time TIME from the TY
C   table.  If ITYRNO = 0 then the TY table will be opened and read.
C   Input:
C    TIME      D    IAT time (days)
C    ANTNO     I    Antenna number
C   Input/Output in common:
C    BIF       I    First IF
C    EIF       I    Last IF
C    ITYRNO    I    Pointer in TY table.
C    SCBUFF(*) I    Buffer for TY
C   Output:
C    TSYST     R(2,*)    Tsys (K) per poln /IF
C    IERR      I    Return error code, 0=>OK, 10=couldn't find.
C   Note: Uses LUN 30.
C-----------------------------------------------------------------------
      INTEGER   ANTNO, IERR
      REAL      TSYST(2,*)
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IP1, IP2, LUN, PLUS1, LOOP, LIMIT, I, SID, SUB,
     *   FQID, NIF
      REAL      WT1, WT2, DT, XTI
      INCLUDE 'ANCAL.INC'
      INCLUDE 'ANCAL2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
      IF (ITYRNO.GT.0) GO TO 100
C                                       Init TY table
      LUN = 30
      CALL TYINI ('READ', SCBUFF, DISKSC, CNOSC, ITYVER, CATBLK, LUN,
     *   ITYRNO, TYKOLS, TYNUMV, NUMPOL, NIF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Get no. records
      NUMTY = SCBUFF(5)
C                                       Find time.
 100  IP1 = 2
      IP2 = 1
C                                       First record
      PLUS1 = ITYRNO
      CALL TABTY ('READ', SCBUFF, PLUS1, TYKOLS, TYNUMV, NUMPOL, NIF,
     *   XTIME(IP2), XTI, SID, ANT(IP2), SUB, FQID, XTSYS(1,1,IP2),
     *   XTANT(1,1,IP2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
      LIMIT = ITYRNO + 1
      DO 300 LOOP = LIMIT,NUMTY
C                                       Flip pointers
         IP1 = 3 - IP1
         IP2 = 3 - IP2
C                                       Right antenna
         PLUS1 = ITYRNO + 1
         CALL TABTY ('READ', SCBUFF, PLUS1, TYKOLS, TYNUMV, NUMPOL, NIF,
     *      XTIME(IP2), XTI, SID, ANT(IP2), SUB, FQID, XTSYS(1,1,IP2),
     *      XTANT(1,1,IP2), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
         IF (ANT(IP1).LT.ANTNO) GO TO 250
         IF (ANT(IP1).GT.ANTNO) GO TO 350
         IF ((TIME.GT.XTIME(IP2)) .AND. (ANTNO.EQ.ANT(IP2))) GO TO 250
         IF (TIME.GE.XTIME(IP1)) GO TO 180
C                                       Before first time
            DO 170 I = 1, EIF - BIF + 1
               TSYST(1,I) = XTSYS(1,I,IP1)
               TSYST(2,I) = XTSYS(2,I,IP1)
 170           CONTINUE
            GO TO 999
 180     IF (ANT(IP1).EQ.ANT(IP2)) THEN
C                                       Between, interpolate
            DT = (XTIME(IP2)-XTIME(IP1))
            IF (DT.LT.1.0E-10) DT = 1.0
            WT1 = (TIME-XTIME(IP1)) / DT
            WT2 = 1.0 - WT1
            DO 190 I = 1, EIF - BIF + 1
               IF ((XTSYS(1,I,IP1).EQ.FBLANK).OR.
     *            (XTSYS(1,I,IP2).EQ.FBLANK)) THEN
                  TSYST(1,I) = FBLANK
               ELSE
                  TSYST(1,I)  = WT2*XTSYS(1,I,IP1) + WT1*XTSYS(1,I,IP2)
                  END IF
               IF ((XTSYS(2,I,IP1).EQ.FBLANK).OR.
     *            (XTSYS(2,I,IP2).EQ.FBLANK)) THEN
                  TSYST(2,I) = FBLANK
               ELSE
                  TSYST(2,I)  = WT2*XTSYS(2,I,IP1) + WT1*XTSYS(2,I,IP2)
                  ENDIF
 190           CONTINUE
            GO TO 999
            END IF
C                                       After last time
            DO 210 I = 1, EIF - BIF + 1
               TSYST(1,I) = XTSYS(1,I,IP1)
               TSYST(2,I) = XTSYS(2,I,IP1)
 210           CONTINUE
            GO TO 999
C                                       Loop back and try next record.
 250     ITYRNO = ITYRNO + 1
 300     CONTINUE
      IF (ANTNO.EQ.ANT(IP2)) THEN
C                                       Must be after last in table
         DO 310 I = I, EIF - BIF + 1
            TSYST(1,I) = XTSYS(1,I,IP2)
            TSYST(2,I) = XTSYS(2,I,IP2)
 310        CONTINUE
         GO TO 999
         END IF
C                                       Something screwed up,
 350   WRITE (MSGTXT,1350) TIME, STNNAM(ANTNO)
       IERR = 10
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETSYS: ERROR ',I3,' OPENING TY TABLE FOR READ')
 1100 FORMAT ('GETSYS: ERROR ',I3,' READING TY TABLE ')
 1350 FORMAT ('GETSYS: COULD NOT FIND TSYS FOR TIME=',F8.5,' ANT=',A)
      END
      SUBROUTINE TYCONS (IERR)
C-----------------------------------------------------------------------
C  Consolidate the TY table - due to the nature of the Tsys ASCII
C  file the source name is not attached to the Tsys's in a predictable
C  manner - therefore after dealing with all the tables we must run
C  back through the TY table and add sourcename info garnered from
C  the CL table
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  IERR
C
      INTEGER  MAXNX
      PARAMETER (MAXNX = 20000)
      INTEGER  NXSRC(MAXNX), NXSUB(MAXNX), NXANT(MAXNX), I, J,  NUMNX,
     *   SUBARR, NUMPLT, NIFTY, SOURID, LUNG, ICLRNO, NUMREC,
     *   CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMNOD, NTERM, KKEY(2,2),
     *   SID, SUB, FQID, TYANT, RECI(13+32*MAXIF), NTYCHN, ANTNO,
     *   KEYSUB(2,2)
      REAL     XTI, TYTIM, GMMOD, FKEY(2,2), RECORD(13+32*MAXIF),
     *   DT, NXTIM(2,MAXNX)
      DOUBLE PRECISION RECD(13+32*MAXIF), TIME
      INCLUDE 'ANCAL.INC'
      INCLUDE 'ANCAL2.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECI, RECD)
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
      IERR = 0
C                                         Open CL table
      LUNG = 28
      CALL CALINI ('READ', SCBUFF, DISKCL, CNOCL, CLVER, CATBLK, LUNG,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, NTERM,
     *   GMMOD, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Set sort keys
      KKEY(1,1) = CLIANT
      KKEY(2,1) = CLIANT
      KKEY(1,2) = CLDTIM
      KKEY(2,2) = CLDTIM
      NUMREC = SCBUFF(5)
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, SCBUFF, SCBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Sort to antenna time order.
      IF ((SCBUFF(43).NE.CLIANT) .OR. (SCBUFF(44).NE.CLDTIM)) THEN
         CALL TABSRT (DISKCL, CNOCL, 'CL', CLVER, CLVER, KKEY, KEYSUB,
     *      FKEY, SCBUFF, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                         Reopen CL table read/write
      CALL CALINI ('READ', SCBUFF, DISKCL, CNOCL, CLVER, CATBLK, LUNG,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, NTERM,
     *   GMMOD, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                      Load up info from CL table
      NUMNX = -1
      DO 150 I = 1, NUMREC
         ICLRNO = I
         CALL TABIO ('READ', 0, ICLRNO, RECORD, SCBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR, 'READ'
            GO TO 990
            END IF
C                                       Get time, etc.
         TIME = RECD(CLKOLS(CLDTIM))
         DT = RECORD(CLKOLS(CLRTMI))
         SOURID = RECI(CLKOLS(CLISID))
         ANTNO = RECI(CLKOLS(CLIANT))
         SUBARR = RECI(CLKOLS(CLISUB))
         IF (NUMNX.EQ.-1) THEN
            NUMNX = 1
            NXTIM(1,NUMNX) = TIME - 0.5 * DT
            NXTIM(2,NUMNX) = TIME + 0.5 * DT
            NXSRC(NUMNX) = SOURID
            NXANT(NUMNX) = ANTNO
            NXSUB(NUMNX) = SUBARR
            END IF
         IF ((SOURID.NE.NXSRC(NUMNX)) .OR. (ANTNO.NE.NXANT(NUMNX))
     *      .OR. (SUBARR.NE.NXSUB(NUMNX))) THEN
            NUMNX = NUMNX + 1
C                                       Check for array boundary error
            IF (NUMNX.GT.MAXNX) THEN
               WRITE (MSGTXT,1015)
               GO TO 990
               END IF
            NXTIM(1,NUMNX) = TIME - 0.5 * DT
            NXTIM(2,NUMNX) = TIME + 0.5 * DT
            NXSRC(NUMNX) = SOURID
            NXANT(NUMNX) = ANTNO
            NXSUB(NUMNX) = SUBARR
            END IF
         NXTIM(2,NUMNX) = TIME + 0.5 * DT
 150     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, SCBUFF, SCBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get the Tsys
C                                       Open Tsys table.
      NTYCHN = 0
      LUNSC = 30
      ITYRNO = 0
      NUMPLT = NUMPOL
      CALL TYINI ('WRIT', SXBUFF, DISKSC, CNOSC, ITYVER, CATBLK,
     *   LUNSC, ITYRNO, TYKOLS, TYNUMV, NUMPLT, NIFTY, IERR)
      IF (IERR.NE.0) GO TO 999
      NUMTY = SXBUFF(5)
C                                         Loop through TY table,
C                                         obtain necessary info from
C                                         NX arrays
      DO 300 I = 1, NUMTY
         ITYRNO = I
         CALL TABTY ('READ', SXBUFF, ITYRNO, TYKOLS, TYNUMV, NUMPLT,
     *      NIFTY, TYTIM, XTI, SID, TYANT, SUB, FQID, XTSYS, XTANT,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            GO TO 990
            END IF
C                                         need to change an entry?
         IF (SID.GT.0) GO TO 300
C                                         find appropriate slot in
C                                         NX arrays
         DO 200 J = 1, NUMNX
            IF ((TYTIM.GE.NXTIM(1,J)) .AND.
     *         (TYTIM.LE.NXTIM(2,J)) .AND. (TYANT.EQ.NXANT(J))) THEN
C                                      found entry
               SID = NXSRC(J)
               SUB = NXSUB(J)
               ITYRNO = I
               NTYCHN = NTYCHN + 1
               CALL TABTY ('WRIT', SXBUFF, ITYRNO, TYKOLS, TYNUMV,
     *            NUMPLT, NIFTY, TYTIM, XTI, SID, TYANT, SUB, FQID,
     *            XTSYS, XTANT, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1030) IERR
                  GO TO 990
                  END IF
               END IF
 200        CONTINUE
 300     CONTINUE
C                                       Close TY table
      CALL TABIO ('CLOS', 0, ITYRNO, SCBUFF, SXBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C
      IF (NTYCHN.GT.NUMTY) NTYCHN = NUMTY
      WRITE (MSGTXT,1040) NTYCHN, NUMTY
      CALL MSGWRT (6)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYCONS: ERROR ',I3,' OPENING CL TABLE')
 1010 FORMAT ('TYCONS: TABIO ERROR =',I3,1X,A4,'ING CL TABLE')
 1015 FORMAT ('TYCONS: PARAMETER MAXNX TOO SMALL')
 1020 FORMAT ('TYCONS: ERROR ',I3,' READING TY TABLE ')
 1030 FORMAT ('TYCONS: ERROR ',I3,' WRITING TY TABLE ')
 1040 FORMAT ('TYCONS: Added Source info to ',I5,' of ',I5,' TY',
     *   ' entries')
      END
      SUBROUTINE CHKORD (VALCH, TORDER, CHANB, CHANE, LIF, HIF, POLNO,
     *                   NCOL)
C-----------------------------------------------------------------------
C  Routine to read the INDEX keyword-values and fill in the TORDER
C  array. TORDER is an index used to sort the TSYS, TANT entries as they
C  are read into the TSYS table. The format of TORDER is:
C
C      TORDER(input) = output + 100*(poln-1)
C
C  where input is the column in the input file, output is the IF column
C  in the TSYS table and poln is either 1(RHC) or 2(LHC).
C
C  Input:
C     VALCH    C(*)*8       Characters from INDEX keyword-value
C  Output:
C     TORDER   I(*)         index
C     CHANB    I            Lowest channel
C     CHANE    I            Highest channel
C     LIF      I            Lowest IF i.e. different freq channels.
C     HIF      I            Lowest IF i.e. different freq channels.
C     POLNO    I            1 if all RHC, 2 if all LHC, 3 if both
C     NCOL     I            The number of columns indexed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER VALCH(*)*8
      INTEGER   TORDER(*), CHANB, CHANE, LIF, HIF, POLNO, NCOL
C
      INTEGER   I, MXIF, POLN, IPT, NIF
      LOGICAL   SOMER, SOMEL
      DATA MXIF /MAXIF/
C-----------------------------------------------------------------------
      CHANB = MXIF
      CHANE = 1
      NIF = 0
      SOMER = .FALSE.
      SOMEL = .FALSE.
      DO 100 I = 1,MXIF
         IF (VALCH(I)(1:1).NE.' ') THEN
            NIF = NIF + 1
            IF (VALCH(I)(1:1).EQ.'X') THEN
               IPT = 2
               CALL CITC2I (VALCH(I), 8, IPT, TORDER(I))
               CHANB = MIN (CHANB, TORDER(I))
               CHANE = MAX (CHANE, TORDER(I))
               TORDER(I) = -1
               GO TO 100
               END IF
            POLN = 0
            IF (VALCH(I)(1:1).EQ.'R') POLN = 1
            IF (VALCH(I)(1:1).EQ.'L') POLN = 2
            IPT = 2
            CALL CITC2I (VALCH(I), 8, IPT, TORDER(I))
            CHANB = MIN (CHANB, TORDER(I))
            CHANE = MAX (CHANE, TORDER(I))
            IF (POLN.EQ.1) SOMER = .TRUE.
            IF (POLN.EQ.2) SOMEL = .TRUE.
            TORDER(I) = TORDER(I) + 100*(POLN-1)
            IF (TORDER(I).LT.0) TORDER(I) = 0
            END IF
 100     CONTINUE
C
      LIF = CHANB
      HIF = CHANE
      CHANE = MAX (CHANE, NIF)
      NCOL = NIF
      IF (SOMER .AND. SOMEL) POLNO = 3
      IF (SOMER .AND. (.NOT.SOMEL)) POLNO = 1
      IF ((.NOT.SOMER) .AND. SOMEL) POLNO = 2
C
 999  RETURN
      END
