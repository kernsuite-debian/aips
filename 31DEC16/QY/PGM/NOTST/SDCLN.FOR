LOCAL INCLUDE 'SDCLN.INC'
C                                                          Include SDCLN
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for APCLN
      INTEGER   LBUFSZ, HBUFSZ, BMHSIZ
C                                       IO buffers: power of 2
      PARAMETER (LBUFSZ = MABFSL)
C                                       Histogram, <= 2 x LBUFSZ
      PARAMETER (HBUFSZ = 16384)
C                                       Histogram of beam size
      PARAMETER (BMHSIZ = 512)
C                                       Local include for SDCLN
      REAL   SCRAT(6), GAIN, FMIN, FLUX, RESMAX, MAPLIM, GAUSA, GAUSB,
     *   GAUSC, GAUSAA, GAUSBB, GAUSCC, PHAT, XSPACE, YSPACE,
     *   MAPROT, SPEXP, TVFMAX, TVFMIN, TVREMX, TVREMN, STPNTS, STFRAC,
     *   STGAIN, BEMVAL(11,11), CLNXST, BUFF1(LBUFSZ), BUFF2(LBUFSZ),
     *   BUFF3(LBUFSZ)
      LOGICAL   NOFIT
      INTEGER   PRECLN, CLNLST, CLNSTR, CLNLIM, PTRJ, PTRK, NX, NY, BPS,
     *   BMHIS(BMHSIZ+2), RESHIS(HBUFSZ), PATCH, MAXPCH, MINPCH, NUMBIN,
     *   LUNBEM, WINB(4), WINM(4,50), NBOXS, LUNRES, LUNRS1, LUNWT,
     *   LUNGD1, LUNGD2, LUNDRT, LUNWRK, LUNCL1, LUNCL2, BEMVOL, RESVOL,
     *   WTVOL, GRDVOL, DRTVOL, WRKVOL, CLNVOL, CONVOL(3), ICENX, ICENY,
     *   CLNVER, BOBEM, BORES, BOWT, BOGRD, BODRT, BOWRK, BOCONV,
     *   CNORES, CNOWT, CNOGRD, CNOWRK, CNOCON(3), BLC(7), NBEMVL, MAXX,
     *   MAXY, MINX, MINY, APSIZ, BUFSZ1, BUFSZ2, BUFSZ3, MAXPIX, BMOFFS
      CHARACTER BEMFIL*48, RESFIL*48, WTFIL*48, GRDFIL*48, DRTFIL*48,
     *   WRKFIL*48, CLNFIL*48, CONFIL(3)*48
      COMMON /CLNCOM/ BUFF1, BUFF2, BUFF3, GAIN, FMIN, FLUX, PHAT,
     *   RESMAX, SCRAT, MAPLIM, GAUSA, GAUSB, GAUSC, GAUSAA, GAUSBB,
     *   GAUSCC, APSIZ, XSPACE, YSPACE, MAPROT, SPEXP, TVFMAX, TVFMIN,
     *   TVREMX, TVREMN, STPNTS, STFRAC, STGAIN, BEMVAL, CLNXST, PRECLN,
     *   CLNLST, CLNSTR, CLNLIM, PTRJ, PTRK, BMHIS, RESHIS, NOFIT,
     *   ICENX, ICENY, NX, NY, PATCH, MAXPCH, MINPCH, NUMBIN, WINB,
     *   WINM, NBOXS, BPS, LUNBEM, LUNRES, LUNRS1, LUNWT, LUNGD1,
     *   LUNGD2, LUNDRT, LUNWRK, LUNCL1, LUNCL2, BEMVOL, RESVOL, WTVOL,
     *   GRDVOL, DRTVOL, WRKVOL, CLNVOL, CONVOL, BOBEM, BORES, BOWT,
     *   BOGRD, BODRT, BOWRK, BOCONV, CNORES, CNOWT, CNOGRD, CNOWRK,
     *   CNOCON, CLNVER, BLC, NBEMVL, MINX, MAXX, MINY, MAXY, BUFSZ1,
     *   BUFSZ2, BUFSZ3, MAXPIX, BMOFFS
      COMMON /COLCHA/ BEMFIL, RESFIL, WTFIL, GRDFIL, DRTFIL, WRKFIL,
     *   CLNFIL, CONFIL
C                                                          End SDCLN
LOCAL END
      PROGRAM SDCLN
C-----------------------------------------------------------------------
C! Modified Steer/Dewdney/Ito CLEAN deconvolution
C# Map AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2003, 2006-2008, 2010, 2015
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
C   SDCLN does a map CLEAN of the type designed by B. Clark, switching
C   to a Steer/Dewdney/Ito method for finding components when that
C   method would subtract > NCOUNT components.
C   INPUTS    ALIAS   DIM(R)                COMMENTS
C   INNAME    NAME(1,1) 3           Dirty map name
C   INCLASS   CLASS(1,1)2           Dirty map class
C   INSEQ     SEQ(1)    1           Dirty map sequence number
C   INDISK    VOL(1)    1           Dirty map volumn
C   BLC       BLC       7           Specifies the plane to CLEAN
C   IN2NAME   NAME(1,2) 3           Beam map name
C   IN2CLASS  CLASS(1,2)2           Beam map class
C   IN2SEQ    SEQ(2)    1           Beam map sequence number
C   IN2DISK   VOL(2)    1           Beam map volumn
C   OUTNAME   NAME(3)   3           Clean map name
C   OUTCLASS  CLASS(3)  2           CLEAN map class
C   OUTSEQ    SEQ(3)    1           CLEAN map sequence number
C   OUTDISK   VOL(3)    1           CLEAN map volumn
C   OUTVER    CLNVER                CLEAN comp. file version #.
C   GAIN                1           CLEAN loop gain
C   FLUX      FMIN      1           Minimum CLEAN component(Jy)
C   PHAT      PHAT                  Prussian hat size.
C
C   NCOUNT    STPNTS    1           Start SDI w # comps
C   SCALR3    STFRAC    1           SDI clip level
C   STFACTOR  STGAIN    1           SDI gain factor
C
C   NITER     CLNLIM    1           Maximum number of CLEAN components
C                                   If neg. stop at first neg. comp.
C   BITER     CLNSTR    1           Restart using first CLSTART comps.
C   BMAJ      GAUSA     1           Major axis of CLEAN restoring beam
C                                   sigma of Gaussian in cells.
C   BMIN      GAUSB     1           Minor axis size of CLEAN beam.
C   BPA       GAUSC     1           Position angle (deg) of BMAJ
C   NBOXES    NBOXS     1           Number of boxes for CLEAN
C   CLBOX(4,50) WINM  200           Windows for the CLEAN boxes.
C   FACTOR    SPEXP     1           Speed up factor
C   MAXPIXEL  MAXPIX    1           Maximum number of pixels searched in
C                                   major cycle.
C   DOTV      ITV       1           1 = request TV display of residual
C                                   maps and ask user if continue,
C                                   < 0 => no display or question
C   BADDISK   IBAD     10           Bad disk list.
C   Programmer = W. D. Cotton 1980, SDI algorithm by Eric W. Greisen
C-----------------------------------------------------------------------
      INTEGER   TVPASS, IDIR, IERR, IRET, USID, VOL(3), SEQ(3), ITV,
     *   CATBLK(256), JBUFS, MAJLOP, KAP, STCOMP, INPCMP, OUTCMP,
     *   INTCMP, NEED, I
      LOGICAL   FINISH, NEW, OLD, T, F, NOSUB, WFIRST, WASCLN, NORES,
     *   DOBEAM, NONEG, ONEPLN, DOSTER, DOSORT
      REAL      CATR(256), HISMAX, TEMP, PLNMAX, PLNMIN, PLNAMX
      CHARACTER HILINE*72, CLASS(3)*6, NAME(3)*12
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DAPM.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATR(1), CATBLK(1))
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Free AP memory
      CALL QRLSE
      INTCMP = 0
      NCFILE = 0
      NSCR = 0
      TVPASS = 0
      FINISH = .FALSE.
      WASCLN = .FALSE.
      DOSORT = .FALSE.
      WFIRST = .TRUE.
      DOSTER = .FALSE.
      STCOMP = 0
      CLNXST = 0.0
      MAJLOP = 0
C                                       Get input values.
      CALL GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IRET)
C                                       Check for restart of AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Set NOSUB flag.
      NOSUB = F
      IF (CLNSTR.LT.0) NOSUB = T
      IF (CLNSTR.LT.0) CLNSTR = -CLNSTR
      IF (NOSUB) CLNLST = CLNSTR
      IF (NOSUB) CLNXST = CLNSTR
      NORES = GAUSA.LT.0.0
      GAUSA = MAX (0.0, GAUSA)
C                                       Set NONEG flag.
      NONEG = CLNLIM.LT.0
      IF (NONEG) CLNLIM = - CLNLIM
C                                       Get map files and create
C                                       output files if required.
      CALL FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Decide if more than one plane
C                                       in image.
      ONEPLN = .NOT. ((CATBLK(KINAX+2).GT.1) .OR. (CATBLK(KINAX+3).GT.1)
     *   .OR. (CATBLK(KINAX+4).GT.1) .OR. (CATBLK(KINAX+5).GT.1)
     *   .OR. (CATBLK(KINAX+6).GT.1))
C                                       Get BEAM if not input.
      IF ((GAUSA.EQ.0.0) .AND. (CATR(KRBMJ).GE.0.0)) THEN
         GAUSA = CATR(KRBMJ) * 3600.
         GAUSB = CATR(KRBMN) * 3600.
         GAUSC = CATR(KRBPA)
         END IF
C                                       If GAUSA=0 fit dirty beam.
      DOBEAM = GAUSA.EQ.0.0
      CALL BMSHP (DOBEAM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write inputs to history and
C                                       log files.
      OLD = (OLD) .AND. (CLNSTR.GT.0)
C                                       OLD if higher plane no. or CC
C                                       file no.
      OLD = OLD .OR. (CLNVER.GT.1)
      CALL CLNHIS (NAME, VOL, CLASS, SEQ, OLD)
C                                       Make WEIGHT file.
      IDIR = 1
      NEED = 2 * NX * NY + NX + NY
      KAP = 8 * NY + 106 + 5 * 10000
      NEED = MAX (NEED, KAP)
      NEED = NEED / 1024
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = 8 * NY + 106 + 5 * 10000 + 4 * NX
         NEED = NEED / 1024
         NEED = MIN (32 * 1024, NEED) + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            IRET = 8
            MSGTXT = 'UNABLE TO GET DESIRED MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
      CALL QCLNSZ (APSIZ)
      MAXPCH = SQRT ((APSIZ-1026.)/2.) - 1
      MAXPCH = MIN (BMHSIZ, MAXPCH) - 1
      I = (APSIZ - BMOFFS - (BMHSIZ+1) * (2*BMHSIZ + 1)) / 3 - 10
      MAXPIX = MIN (MAXPIX, I)
      CALL APDFFT (IDIR, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL QRLSE
C                                       See if restore only requested.
      IF (CLNSTR.GE.CLNLIM) FINISH = .TRUE.
      IF (CLNSTR.GE.CLNLIM) GO TO 200
      WASCLN = .TRUE.
C                                       Compute BEAM histogram.
      CALL HISTOB (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       If restarting sub. clean comp.
      NEW = .TRUE.
      IF ((CLNSTR.GT.0) .AND. (.NOT.NOSUB)) THEN
         MAJLOP = 1
         WRITE (MSGTXT,1025) CLNSTR
         CALL MSGWRT (4)
         CLNXST = CLNSTR
         NEW = .FALSE.
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'UNABLE TO GET AP MEMORY BACK 1'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 990
            END IF
         CALL GRIDER (WFIRST, IRET)
         IF (IRET.NE.0) GO TO 990
         WFIRST = F
         IDIR = -1
         CALL APDFFT (IDIR, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Release and reaquire AP
         CALL QRLSE
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'UNABLE TO GET AP MEMORY BACK 2'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 990
            END IF
C                                       Make pass through ADDMAP to get
C                                       maximum
         CALL ADDMAP (NEW, PLNMAX, PLNMIN, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL QRLSE
C                                       Determine new RESMAX.
         RESMAX = MAX (ABS(TVREMX), ABS(TVREMN))
         END IF
C                                       Initialize RESIDUAL map.
      NOSUB = F
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'UNABLE TO GET AP MEMORY BACK 3'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 990
         END IF
 60   CALL ADDMAP (NEW, PLNMAX, PLNMIN, IRET)
      IF (IRET.NE.0) GO TO 990
      TEMP = MAX (ABS(TVREMX), ABS(TVREMN))
C                                       Check if histogram max. OK.
      IF ((NEW) .AND. (TEMP.LE.(0.9*ABS (RESMAX)))) THEN
C                                       Redo ADDMAP for better
C                                       histogram.
         WRITE (MSGTXT,1060) TEMP, RESMAX
C                                       Warn only if really a problem
         PLNAMX = MAX (ABS (PLNMAX), ABS (PLNMIN))
         IF (TEMP.LT.(0.9*PLNAMX)) CALL MSGWRT (8)
         RESMAX = TEMP
         GO TO 60
         END IF
C                                       Display current residual map.
      IF (ITV.EQ.1) CALL DISPTV (TVPASS)
      IF (ITV.NE.1) CALL QRLSE
      IF (ITV.EQ.1) TVPASS = 2
C                                       Begin CLEANing.
 100  CONTINUE
         CALL TOUCH
         CALL SDITEL (FINISH, NORES, NONEG, ITV, IRET)
         IF (IRET.NE.0) GO TO 990
         IF (ITV.NE.1) TVPASS = 1
         IF (FINISH) THEN
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'UNABLE TO GET AP MEMORY BACK 4'
               CALL MSGWRT (8)
               IRET = 8
               GO TO 990
               END IF
            IF (DOSORT) GO TO 190
            GO TO 210
            END IF
         CALL DECIDE (DOSTER)
C                                       BGC (normal) CLEAN
         IF (.NOT.DOSTER) THEN
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'UNABLE TO GET AP MEMORY BACK 5'
               CALL MSGWRT (8)
               IRET = 8
               GO TO 990
               END IF
            CALL MAPPAK (IRET)
            IF (IRET.NE.0) GO TO 990
            CALL BMSHOV (IRET)
            IF (IRET.NE.0) GO TO 990
            CALL ACLN (NONEG, FINISH, IRET)
            IF (IRET.NE.0) GO TO 990
            HISMAX = RESMAX
            CALL QRLSE
            DOSORT = .TRUE.
C                                       Steer/Dewdney/Ito clean
         ELSE
            IF (STCOMP.LE.0) THEN
               STCOMP = CLNSTR + 1
               WRITE (MSGTXT,1120) STCOMP
               CALL MSGWRT (4)
               END IF
            CALL SDICLN (NONEG, FINISH, IRET)
            IF (IRET.NE.0) GO TO 990
            HISMAX = RESMAX
            DOSORT = .TRUE.
            END IF
C                                       Go back to dirty image?
 190     MAJLOP = MAJLOP + 1
         IF ((WFIRST) .OR. (.NOT.DOSORT)) GO TO 200
C                                       Compress CC file
         IF ((MAJLOP.LT.10) .AND. (.NOT.FINISH)) GO TO 200
            JBUFS = 8192
            INPCMP = 0
            OUTCMP = 0
            CALL CCMERG (CLNVOL, FCNO(3), CLNVER, CLNVER, INPCMP,
     *         OUTCMP, JBUFS, BUFF1, IRET)
            INTCMP = INTCMP + INPCMP - OUTCMP
            CLNSTR = OUTCMP
            CLNLST = 0
            WFIRST = .TRUE.
            MAJLOP = 1
            WRITE (MSGTXT,1190) INPCMP, OUTCMP
            IF (IRET.NE.0) WRITE (MSGTXT,1191) IRET
            CALL MSGWRT (4)
            IF (IRET.NE.0) GO TO 990
            DOSORT = .FALSE.
C                                       Create grid file
 200  CONTINUE
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'UNABLE TO GET AP MEMORY BACK 6'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 990
            END IF
         CALL GRIDER (WFIRST, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       WFIRST = .TRUE. iff first write
C                                       onto grid file.
         WFIRST = F
C                                       Restore if finished.
 210     CALL SDITEL (FINISH, NORES, NONEG, ITV, IRET)
         IF (IRET.NE.0) GO TO 990
         IF (ITV.NE.1) TVPASS = 1
         IF ((DOSORT) .AND. (FINISH)) GO TO 190
         IF ((FINISH) .AND. (.NOT.NORES)) CALL RESTOR (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       FFT to map plane.
         IDIR = -1
         CALL APDFFT (IDIR, IRET)
         CALL QRLSE
         IF (IRET.NE.0) GO TO 990
C                                       Reaquire AP
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'UNABLE TO GET AP MEMORY BACK 7'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 990
            END IF
C                                       Subtract from dirty map.
         CALL ADDMAP (F, PLNMAX, PLNMIN, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Determine RESMAX if finished.
         IF (FINISH) THEN
            RESMAX = MAX (ABS (PLNMAX), ABS (PLNMIN))
C                                       Display current residual map.
            IF (TVPASS.GE.2) TVPASS = 3
            IF (TVPASS.EQ.1) TVPASS = 0
            TVREMX = PLNMAX
            TVREMN = PLNMIN
            TVFMAX = TVREMX
            TVFMIN = TVREMN
            END IF
         IF (ITV.EQ.1) CALL DISPTV (TVPASS)
         IF (ITV.NE.1) CALL QRLSE
         IF (TVPASS.EQ.32700) THEN
            FINISH = .TRUE.
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'UNABLE TO GET AP MEMORY BACK 8'
               CALL MSGWRT (8)
               IRET = 8
               GO TO 990
               END IF
            IF (DOSORT) GO TO 190
            GO TO 210
            END IF
         IF (ITV.EQ.1) TVPASS = 2
C                                       Update CLEAN catalog header
         CATBLK(KINIT) = CLNSTR
         CATR(KRBMJ) = GAUSA / 3600.
         CATR(KRBMN) = GAUSB / 3600.
         CATR(KRBPA) = GAUSC
         CATBLK(KITYP) = 1
         CATR(KRDMX) = MAX (CATR(KRDMX), PLNMAX)
         CATR(KRDMN) = MIN (CATR(KRDMN), PLNMIN)
         IF (ONEPLN) CATR(KRDMX) = PLNMAX
         IF (ONEPLN) CATR(KRDMN) = PLNMIN
         IF (NORES) CATBLK(KITYP) = 3
         IF ((GAUSA.GT.0.0) .AND. (GAUSA.LT.1.E-5)) CATBLK(KITYP) = 4
         CALL CATIO ('UPDT', VOL(3), CCNO, CATBLK, 'REST', BUFF3,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
            WRITE (MSGTXT,1200) IERR
            CALL MSGWRT (8)
            END IF
C                                       If more cleaning required,
C                                       loop.
         IF (.NOT.FINISH) GO TO 100
C                                       Clear  map files
      IRET = IERR
      IF ((IERR.NE.0) .AND. (IERR.LT.9)) GO TO 990
C                                       History
      CALL HIOPEN (LUNCL1, VOL(3), CCNO, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 290
C                                       Plane number
      WRITE (HILINE,1259) TSKNAM, BLC(3)
      IF (BLC(3).GT.1) CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 290
C                                        Write total CLEANed flux
      WRITE (HILINE,1255) TSKNAM, FLUX
      CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 290
C                                       Tell if comps. NOT restored
      WRITE (HILINE,1258) TSKNAM
      IF (NORES) CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 290
C                                       Write number of iterations
C                                       actually done.
      WRITE (HILINE,1256) TSKNAM, CLNSTR
      CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 290
      IF (WASCLN) THEN
         WRITE (HILINE,1257) TSKNAM, HISMAX
         CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 290
         END IF
      IF (STCOMP.GT.0) THEN
         WRITE (HILINE,1260) TSKNAM, STCOMP
         CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 290
         END IF
      IF ((INPCMP.GT.0) .AND. (OUTCMP.GT.0)) THEN
         INTCMP = INTCMP + OUTCMP
         WRITE (HILINE,1265) TSKNAM, INTCMP, OUTCMP
         CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 290
         END IF
 290  CALL HICLOS (LUNCL1, T, BUFF1, IERR)
C                                        Finished.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1025 FORMAT ('Restart CLEAN at comp ',I8)
 1060 FORMAT ('*** WARNING: ABS(WINDOW) MAX =',F9.5,' ABS(MAP) MAX =',
     *   F9.5)
 1120 FORMAT ('Begin Steer/Dewdney/Ito method at component',I8)
 1200 FORMAT ('ERROR',I3,' UPDATING CLEAN HEADER ')
 1190 FORMAT ('Compressed',I10,' components into',I10,' components')
 1191 FORMAT ('ERROR',I7,' COMPRESSING CC FILE')
 1255 FORMAT (A6,' / Total CLEANed flux density = ',1PE12.4,' Jy')
 1256 FORMAT (A6,' / Number of iterations done =',I8)
 1257 FORMAT (A6,' / Peak residual =',1PE12.4,' Jy/beam')
 1258 FORMAT (A6,' / Components not restored to image')
 1259 FORMAT (A6,' / CLEAN of plane ',I4)
 1260 FORMAT (A6,' / SDI method begun at component',I9)
 1265 FORMAT (A6,' / Compress',I10,' total components into',I10)
      END
      SUBROUTINE ACLN (NONEG, FINISH, IERR)
C-----------------------------------------------------------------------
C   ACLN does a limited clean of the Barry Clark variety using the
C   highest map points and limited beam stored in the AP.
C   INPUT:
C     NONEG  L     IF .TRUE. then stop at first negative component.
C     LUNCL1 I   = Logical unit number for reading the clean components.
C     LUNCL2 I   = Logical unit number for writing the clean components.
C     CLNFIL C   = File name array for the clean component file.
C     GAIN   R   = Clean loop gain.
C     PTRK   I   = Number of map points in the AP.
C     PTRJ   I   = first location in the AP of the beam.
C     CLNSTR I   = First clean component to do. (0 for first clean).
C     CLNLIM I   = Maximum number of clean components desired.
C
C     OUTPUT:
C     CLNSTR I   = Current clean component number.
C     FINISH L   = .TRUE. If minimum clean component flux density
C                  or maximum iteration encountered, otherwise
C                  = .FALSE.
C     RESMAX R   = Maximum abs. residual (Jy).
C     A list of the clean components on file CLNFIL.
C-----------------------------------------------------------------------
      CHARACTER KEYS(4)*8, PREFIX*5
      LOGICAL   NONEG, FINISH, LERR, FA
      INTEGER   ITER, JPTR, NCOMPC, LENCBU, I, APFDV, COMP, IBUFF(512),
     *   NREC, CATBLK(256), IRET, IERR, LREC, NCBUFF, IPOINT, NKEY,
     *   KOLS(4), RAKOL, DECKOL, FLXKOL, TYPKOL
      REAL      ATLIM, RECORD(7), LITER, XFAC, XFLUX, XPROD,
     *   CATR(256), XCENTR, YCENTR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK(1), CATR(1))
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL)
      EQUIVALENCE (BUFF1(1), IBUFF(1))
C                                       LENCBU, NCOMPC control the
C                                       no. CC per AP call.
      DATA LENCBU, NCOMPC /137, 32/
      DATA FA /.FALSE. /
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ',
     *   'TYPE OBJ'/
      DATA APFDV, COMP /2, 9/
C-----------------------------------------------------------------------
C                                       setup values in AP
C                                       beam patch size
      BUFF2(1) = PATCH
      BUFF2(2) = PATCH
C                                       Field descriptor vector
      BUFF2(3) = 1.0
      BUFF2(4) = BMOFFS
      BUFF2(5) = PTRK
      BUFF2(6) = 0.0
      BUFF2(7) = 0.0
      BUFF2(8) = 0.0
      BUFF2(9) = GAIN
C                                       Components vector
      BUFF2(10) = 0.0
      BUFF2(11) = 0.0
      BUFF2(12) = 0.0
      BUFF2(13) = 0.0
      CALL QPUT (BUFF2, 0, 13, 2)
C                                       put address as integer
      I = BMOFFS
      CALL QPUT (I, 3, 1, 1)
C                                       Open CLNFIL for reading
      NREC = 5000
      IF (CLNLIM-CLNSTR.LT.4990)  NREC = CLNLIM - CLNSTR + 10
      LREC = 3
      NKEY = 1
      CALL TABINI ('WRIT', 'CC', CLNVOL, FCNO(3), CLNVER, CATBLK,
     *   LUNCL2, NKEY, NREC, LREC, BUFF2, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CLNSTR = CLNSTR + 1
      NKEY = 3
      TYPKOL = 7
      FLXKOL = 1
      RAKOL = 2
      DECKOL = 3
      IF (LREC.GT.3) NKEY = 4
      CALL FNDCOL (NKEY, KEYS, 8, FA, IBUFF, KOLS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1016) IRET
         CALL MSGWRT (7)
         END IF
      CALL RFILL (7, 0.0, RECORD)
      XCENTR = CATR(KRCRP)
      YCENTR = CATR(KRCRP+1)
C                                       Prepare to CLEAN
      JPTR = PTRJ
      CALL QWD
C                                       Start CLEANing.
      CALL QMULCL (COMP, APFDV, JPTR, 0, NCOMPC)
      ITER = CLNSTR
      LITER = CLNXST
C                                       Speed up clean.
      XFAC = MAPLIM / RESMAX
      IF (XFAC.LT.0.0) XFAC = -XFAC
      XPROD = XFAC**SPEXP
      XFAC = XPROD
      ATLIM = 0.0
      CALL QWR
      NCBUFF = NCOMPC
C                                       Begin loop.
 30   NCBUFF = NCBUFF + 1
C                                       See if time to get more from
C                                       the AP.
         IF (NCBUFF.GT.NCOMPC) THEN
            CALL QWR
C                                       Check AP roller.
            CALL QROLL (APSIZ, BUFF3, BUFSZ3, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Get next load of components.
            CALL QGET (BUFF2, 0, LENCBU, 2)
C                                       Load last comp. for CLEAN.
            CALL QWD
C                                       Start CLEANing again.
            CALL QMULCL (COMP, APFDV, JPTR, 0, NCOMPC)
            NCBUFF = 1
            END IF
C                                       Process a Component.
         IPOINT = 10 + (NCBUFF-1) * 4
C                                       Get component
         RECORD(TYPKOL) = 0
         RECORD(RAKOL) = (BUFF2(IPOINT+1) - XCENTR) * XSPACE
         RECORD(DECKOL) = (BUFF2(IPOINT+2) - YCENTR) * YSPACE
         FLUX = FLUX + BUFF2(IPOINT) * GAIN
C                                       Stored RA and Dec refer to
C                                       the catalogd CLEAN map.
         RECORD(FLXKOL) = BUFF2(IPOINT) * GAIN
         CALL TABIO ('WRIT', 0, ITER, RECORD, IBUFF, IERR)
         IF (IERR.EQ.4) THEN
            WRITE (MSGTXT,1030) ITER
            CALL MSGWRT (8)
            END IF
         IF (IERR.NE.0) GO TO 999
         LITER = LITER + 1.0
         XFLUX = BUFF2(IPOINT)
         CALL METSCA (XFLUX, PREFIX, LERR)
C                                       Check for iteration limit.
         IF (LITER.GE.CLNLIM) THEN
            WRITE (MSGTXT,1110) ITER, XFLUX, PREFIX
            CALL MSGWRT (4)
C                                       Check for minimum CLEAN flux
C                                       Check for negative comp.
         ELSE IF ((ABS(BUFF2(IPOINT)).LT.FMIN) .OR.
     *      ((NONEG) .AND. (BUFF2(IPOINT).LT.0.0))) THEN
            WRITE (MSGTXT,1090) XFLUX, PREFIX, ITER
            CALL MSGWRT (4)
C                                       Check minimum algorithm flux
C                                       Don't stop in last 5%
         ELSE IF ((ABS(BUFF2(IPOINT)).LT.MAPLIM*(1.0+ATLIM)) .AND.
     *      (LITER.LT.0.95*CLNLIM)) THEN
            WRITE (MSGTXT,1100) XFLUX, PREFIX, ITER
            CALL MSGWRT (4)
C                                       Loop back for next clean.
         ELSE
            ITER = ITER + 1
C                                       Include no. comps. sub from
C                                       the dirty map.
            ATLIM = ATLIM + XFAC / (ITER + PRECLN)
            GO TO 30
            END IF
C                                       Save maximum residual
      RESMAX = ABS (BUFF2(IPOINT))
C                                       See if finished.
      FINISH = .FALSE.
      IF (LITER.GE.CLNLIM) FINISH = .TRUE.
      IF (RESMAX.LE.FMIN) FINISH = .TRUE.
      IF (NONEG.AND.(BUFF2(IPOINT).LT.0.0)) FINISH = .TRUE.
C                                       Write total flux density
      XFLUX = FLUX
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1120) XFLUX, PREFIX
      CALL MSGWRT (4)
C                                       Output last buffer and close.
      CALL TABIO ('CLOS', 0, ITER, RECORD, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         CALL MSGWRT (8)
         END IF
C                                       Set CLNSTR to the current clean
C                                       counter (NCOMP - 1 ).
      CLNSTR = ITER
      CLNXST = LITER
      ITER = CLNXST  + 0.5
      WRITE (MSGTXT,1140) ITER
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACLN: ERROR',I3,' OPENING FILE ')
 1015 FORMAT ('ACLN: ERROR',I3,' CLOSING FILE ')
 1016 FORMAT ('ACLN: ERROR',I3,' FINDING REQUIRED CC COLUMNS')
 1030 FORMAT ('ACLN: ITER. ',I8,' CC FILE LIMIT EXCEEDED')
 1090 FORMAT ('Reached min. CLEAN flux density=',F8.3,1X,A5,'Jy',
     *   ' iter=',I8)
 1100 FORMAT ('Reached min. algorithm flux =',F8.3,1X,A5,'Jy',
     *   ' iter =',I8)
 1110 FORMAT ('Reached iteration limit',I8,' max. res=',F8.3,1X,A5,
     *   'Jy')
 1120 FORMAT ('Total CLEANed flux density =',F8.3,1X,A5,'Jy')
 1140 FORMAT ('at effective iteration =',I10)
      END
      SUBROUTINE ADDMAP (FIRST, RMAX, RMIN, IERR)
C-----------------------------------------------------------------------
C   ADDMAP adds the transformed GRID map to the Dirty map producing
C   the residual map or the CLEAN  map depending on whether or not
C   RESTOR was run. If FIRST is TRUE then the dirty map is copied
C   to the residual map file.
C   The transformed grid file is assumed to be in the WRK file.
C   Also computes the histogram of residual map values.
C   INPUT:
C     FIRST  L     If TRUE the dirty map is copied to the residual map.
C   OUTPUT:
C     RMAX    R   Maximum in residual/restored map.
C     RMIN    R   Minimum in residual/restored map.
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, WIN(4),
     *   I, IBOX, IERR, J, LIMIT
      INTEGER   IAPSCL, IAPMAX, IAPMIN, IAPRES, PNUMIN, IAPHIS, IAPHMX,
     *   IAPHMN, ISTART, SIZE, IAPSUM, MX, ONENX
      LOGICAL   FIRST, MAP, EXCL, WAIT
      REAL      RMAX, RMIN, TMAX, TMIN, XTEMP(8), YTEMP(3), TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMAX, YTEMP(1)),      (TMIN, YTEMP(3))
      DATA MAP, EXCL, WAIT /.TRUE., 2*.TRUE./
      DATA IAPSCL, IAPMAX, IAPMIN, IAPHMX, IAPHMN, IAPRES
     *   /    0,      2,      4,      6,      7,      10/
C-----------------------------------------------------------------------
C                                       Initialize extrema.
      RMAX = -1.0E20
      RMIN =  1.0E20
      TVREMX = RMAX
      TVREMN = RMIN
C                                       Setup AP locations.
C                                         0 =
C                                         1 =
C                                         2 = MAX    (IAPMAX)
C                                         4 = MIN    (IAPMIN)
C                                         6 = Hist. max. (IAPHMX)
C                                         7 = Hist. min. (IAPHMN)
C                                        10 = Residual map (IAPRES)
C                                        IAPRES+NX+1 Dirty & Sum(IAPSUM)
C                                        IAPSUM+NX+1 Histogram (IAPHIS)
      ONENX = NX
      PNUMIN = NUMBIN + 1
      SIZE = ONENX + 1
      IAPSUM = IAPRES + SIZE
      IAPHIS = IAPSUM + SIZE
C                                       Clear AP area for Residual map.
      CALL QVCLR (IAPRES, 1, ONENX)
C                                       Clear Histogram.
      CALL QVCLR (IAPHIS, 1, PNUMIN)
C                                       Open and INIT files.
      CALL ZOPEN (LUNDRT, FIND1, DRTVOL, DRTFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      CALL ZOPEN (LUNWRK, FIND2, WRKVOL, WRKFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      CALL ZOPEN (LUNRES, FIND3, RESVOL, RESFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      CALL MINIT ('READ', LUNDRT, FIND1, NX, NY, WIN, BUFF1, BUFSZ1,
     *   BODRT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      CALL MINIT ('READ', LUNWRK, FIND2, NX, NY, WIN, BUFF2, BUFSZ2,
     *   BOWRK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      CALL MINIT ('WRIT', LUNRES, FIND3, NX, NY, WIN, BUFF3, BUFSZ3,
     *   BORES, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Load Hist maxmin
      XTEMP(1) = 1.0
      XTEMP(2) = 0.0
      XTEMP(7) = RESMAX
      XTEMP(8) = 0.0
      CALL QPUT (XTEMP, IAPSCL, 8, 2)
C                                       Loop thru map.
      DO 150 I = 1,NY
C                                       Read dirty map.
         CALL MDISK ('READ', LUNDRT, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR,I
            GO TO 990
            END IF
C                                       Write output.
         CALL MDISK ('WRIT', LUNRES, FIND3, BUFF3, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR,I
            GO TO 990
            END IF
C                                       If FIRST = .FALSE. read residual
         IF (.NOT.FIRST) THEN
            CALL MDISK ('READ', LUNWRK, FIND2, BUFF2, BIND2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1105) IERR,I
               GO TO 990
               END IF
            END IF
         CALL QWR
         CALL QPUT (BUFF1(BIND1), IAPSUM, ONENX, 2)
         CALL QWD
C                                       Load residual row (FIRST=false)
         IF (.NOT.FIRST) THEN
            CALL QWR
            CALL QPUT (BUFF2(BIND2), IAPRES, ONENX, 2)
            CALL QWD
C                                       Add rows.
            CALL QVADD (IAPSUM, 1, IAPRES, 1, IAPSUM, 1, ONENX)
            END IF
C                                       Find max and min in row.
         CALL QMAXV (IAPSUM, 1, IAPMAX, ONENX)
         CALL QMINV (IAPSUM, 1, IAPMIN, ONENX)
C                                       Read results.
         CALL QWR
         CALL QGET (BUFF3(BIND3), IAPSUM, ONENX, 2)
         CALL QGET (YTEMP, IAPMAX, 3, 2)
         CALL QWD
C                                       Note use of EQUIVALENCE.
C                                       Save max and min.
         RMAX = MAX (RMAX, TMAX)
         RMIN = MIN (RMIN, TMIN)
C                                       Compute histogram of residuals.
         DO 120 IBOX = 1,NBOXS
            IF ((I.GE.WINM(2,IBOX)) .AND. (I.LE.WINM(4,IBOX))) THEN
               ISTART = IAPSUM + WINM(1,IBOX) - 1
               MX = (WINM(3,IBOX) - WINM(1,IBOX)) + 1
               IF (MX.LT.0) MX = - MX + 2
               CALL QMAXV (ISTART, 1, IAPMAX, MX)
               CALL QMINV (ISTART, 1, IAPMIN, MX)
               CALL QWR
               CALL QGET (YTEMP, IAPMAX, 3, 2)
               CALL QWD
               TVREMN = MIN (TVREMN, TMIN)
               TVREMX = MAX (TVREMX, TMAX)
               CALL QVABS (ISTART, 1, ISTART, 1, MX)
               CALL QHIST (ISTART, 1, IAPHIS, MX, PNUMIN, IAPHMX,
     *            IAPHMN)
               END IF
 120        CONTINUE
 150     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUNRES, FIND3, BUFF3, BIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1150) IERR
         GO TO 990
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNDRT, FIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1160) IERR
         CALL MSGWRT (8)
         END IF
      CALL ZCLOSE (LUNWRK, FIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1170) IERR
         CALL MSGWRT (8)
         END IF
      CALL ZCLOSE (LUNRES, FIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1180) IERR
         CALL MSGWRT (8)
         END IF
C                                       Get histogram from AP.
      CALL QWR
      CALL QGET (BUFF2, IAPHIS, PNUMIN, 2)
      CALL QWD
C                                       Integrate histogram.
      BUFF2(NUMBIN) = BUFF2(NUMBIN) + BUFF2(NUMBIN+1)
      RESHIS(NUMBIN) = BUFF2(NUMBIN)
      LIMIT = NUMBIN - 1
      DO 200 I = 1,LIMIT
         J = NUMBIN - I + 1
         TEMP = RESHIS(J)
         TEMP = TEMP + BUFF2(J-1)
         RESHIS(J-1) = TEMP + 0.5
 200     CONTINUE
      RESHIS(NUMBIN+1) = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ADDMAP: CANNOT OPEN DIRTY FILE, ERROR ',I3)
 1020 FORMAT ('ADDMAP: CANNOT OPEN FOR READ RESIDUAL MAP, ERROR ',I3)
 1030 FORMAT ('ADDMAP: CANNOT OPEN FOR WRITE RESIDUAL MAP, ERROR ',I3)
 1040 FORMAT ('ADDMAP: CANNOT INIT DIRTY MAP, ERROR ',I3)
 1050 FORMAT ('ADDMAP: CANNOT INIT READ RESIDUAL MAP, ERROR ',I3)
 1060 FORMAT ('ADDMAP: CANNOT INIT WRITE RESIDUAL MAP, ERROR ',I3)
 1070 FORMAT ('ADDMAP: READ ERROR ',I3,' DIRTY FILE ROW ',I6)
 1105 FORMAT ('ADDMAP: READ ERROR ',I3,' RESIDUAL MAP ROW ',I6)
 1100 FORMAT ('ADDMAP: WRITE ERROR ',I3,' RESIDUAL ROW ',I6)
 1150 FORMAT ('ADDMAP: FINISH ERROR ',I3,' RESIDUAL FILE')
 1160 FORMAT ('ADDMAP: CLOSE ERROR ',I3,' DIRTY MAP ')
 1170 FORMAT ('ADDMAP: CLOSE ERROR ',I3,' READ RESIDUAL FILE')
 1180 FORMAT ('ADDMAP: CLOSE ERROR ',I3,' WRITE RESIDUAL FILE ')
      END
      SUBROUTINE APDFFT (IDIR, IERR)
C-----------------------------------------------------------------------
C   APDFFT calls AP FFT routines PASS1 and PASS2.  If the transform
C   can be done all in the AP this is done; if not then a transpose
C   of the type designed by F. Schwab is done.
C   For the forward transform the Beam map is tranformed to the
C   weight file and on the reverse transform the GRID file is
C   transformed to the WORK file. The contents of the GRID file are
C   destroyed.
C   The beam in R   form is assumed in file GRD.
C   INPUT:
C     IDIR       I    1 for forward transform, -1 for reverse,
C     BOGRD      I    Block offset of beam map.
C     GRDVOL     I    Volume for beam map.
C     GRDFIL     C    Physical name of  Beam map.
C     LUNGD1     I    LUN for Beam map.
C     BOGRD      I    Block offset of GRID file.
C     GRDVOL     I    Volume for GRID file.
C     GRDFIL     C    Physical name of GRID file.
C     LUNGD1     I    LUN for GRID file.
C     BOWT       I    Block offset of weight file.
C     WTVOL      I    Volume for weight file.
C     WTFIL      C    Physical name of weight file.
C     LUNWT      I    LUN for weight file.
C     BORES      I    Block offset for residual map.
C     RESVOL     I    Volume for residual map.
C     RESFIL     C    Physical name of residual map.
C     LUNRES     I    LUN for residual map.
C     BOWRK      I    Block offset for WORK file.
C     WRKVOL     I    Volume for work file.
C     WRKFIL     C    Physical name of WORK file.
C     LUNWRK     I    LUN for WORK file.
C     BUFF1(),BUFF2()  I    Work buffers for I/O.
C     BUFSZ1,BUFSZ2    I    Size in bytes of BUFF1 and BUFF2.
C-----------------------------------------------------------------------
      INTEGER   IERR, JDIR
      INTEGER   IDIR, LUN(3), BO(3), VOL(3)
      LOGICAL   FULL
      REAL      SMAX, SMIN
      CHARACTER FIL(3)*48
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA FULL /.FALSE./
C-----------------------------------------------------------------------
C                                       REAL TO COMPLEX
C                                       Set file information.
C                                       1 = Beam, 2 = Work, 3 = Weight
      IF (IDIR.GE.0) THEN
         BO(1) = BOGRD
         BO(2) = BOWRK
         BO(3) = BOWT
         VOL(1) = GRDVOL
         VOL(2) = WRKVOL
         VOL(3) = WTVOL
         LUN(1) = LUNGD1
         LUN(2) = LUNWRK
         LUN(3) = LUNWT
         FIL(1) = GRDFIL
         FIL(2) = WRKFIL
         FIL(3) = WTFIL
C                                       Do disk based FFT.
         JDIR = 3
         CALL PASS1 (JDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (JDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, SMAX, SMIN, IERR)
C                                       COMPLEX TO REAL
C                                       Set file information.
C                                       1 = Grid, 2 = Resid, 3 = Work
      ELSE
         BO(1) = BOGRD
         BO(2) = BORES
         BO(3) = BOWRK
         VOL(1) = GRDVOL
         VOL(2) = RESVOL
         VOL(3) = WRKVOL
         LUN(1) = LUNGD1
         LUN(2) = LUNWRK
         LUN(3) = LUNRES
         FIL(1) = GRDFIL
         FIL(2) = RESFIL
         FIL(3) = WRKFIL
C                                       Do disk based FFT.
         CALL PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, SMAX, SMIN, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE BMSHOV (IERR)
C-----------------------------------------------------------------------
C   BMSHOV loads the beam patch into the array processor.
C   Input :
C      LUNBEM I   = Logical unit number for the beam map file.
C      BEMFIL I   = Name of the beam map file.
C      PATCH  I   = Beam patch half size (cells).
C      BOBEM  I   = Blok offset for the beam map.
C      APSIZ  I   = Size of array processor in words.
C      PTRJ   I   = AP location of beginning of beam.
C      ICENX  I   = Offset of center of beam map in X from NX/2+1
C      ICENY  I   = Offset of center of beam map in Y from NY/2+1
C-----------------------------------------------------------------------
      INTEGER   FIND, BIND, IERR, I, MSIZE
      INTEGER   IPTR, PPATCH, TWOPAT, CORE
      LOGICAL   MAP, EXCL, WAIT
      REAL      RTEMP, TEMP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA MAP, EXCL, WAIT /3*.TRUE./
C-----------------------------------------------------------------------
C                                      Set window for beam patch.
      WINB(1) = NX / 2 + 1 + ICENX
      WINB(2) = NY / 2 - PATCH + 2 + ICENY
      WINB(3) = NX / 2 + PATCH + ICENX
      WINB(4) = NY / 2 + PATCH + ICENY
      PPATCH = PATCH
      TWOPAT = 2 * PATCH - 1
      TEMP(1) = 1.0
      TEMP(2) = 0.0
      CALL QPUT (TEMP, 0, 2, 2)
C                                      Check to make sure the beam
C                                      patch will fit in the AP.
      CORE = PTRJ + PPATCH * (2 * PPATCH - 1)
      IF (CORE.GE.APSIZ) THEN
         WRITE (MSGTXT,1000) CORE
         GO TO 990
         END IF
C                                      Open beam file.
      CALL ZOPEN (LUNBEM, FIND, BEMVOL, BEMFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         GO TO 990
         END IF
C                                      Initialize beam map.
      CALL MINIT ('READ', LUNBEM, FIND, NX, NY, WINB, BUFF1, BUFSZ1,
     *   BOBEM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Loop thru beam & shove into AP
      IPTR = PTRJ
      MSIZE = 2 * PATCH - 1
      DO 30 I = 1,MSIZE
C                                      Read row of the map.
         CALL MDISK ('READ', LUNBEM, FIND, BUFF1, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR, I
            GO TO 990
            END IF
C                                      Load row into AP.
         CALL QWR
         CALL QPUT (BUFF1(BIND), 2, PPATCH, 2)
         CALL QWD
         CALL QVMOV (2, 1, IPTR, TWOPAT, PPATCH)
         IPTR = IPTR + 1
C                                       Add offset to center of beam
         IF ((I.EQ.PATCH) .AND. (PHAT.GE.1.0E-10)) THEN
            CALL QWR
            CALL QGET (RTEMP, IPTR, 1, 2)
            WRITE (MSGTXT,1025) RTEMP
            CALL MSGWRT (4)
            RTEMP = 1.0 + PHAT
            CALL QPUT (RTEMP, IPTR, 1, 2)
            CALL QGET (RTEMP, IPTR, 1, 2)
            CALL QWD
            WRITE (MSGTXT,1026) RTEMP
            CALL MSGWRT (4)
            END IF
 30      CONTINUE
C                                      Close beam file.
      CALL ZCLOSE (LUNBEM, FIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         END IF
C                                       Roll AP if necessary.
C                                       Save all of AP memory.
      CALL QROLL (APSIZ, BUFF1, BUFSZ1, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BMSHOV: YOU NEED ',I8,' WORDS OF AP CORE ')
 1005 FORMAT ('BMSHOV: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('BMSHOV: ERROR',I3,' INIT FILE ')
 1015 FORMAT ('BMSHOV: READ ERROR',I3,' ROW ',I5)
 1025 FORMAT ('BMSHOV: Old beam peak = ',1PE12.4)
 1026 FORMAT ('BMSHOV: New beam peak = ',1PE12.4)
 1030 FORMAT ('BMSHOV: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE BMSHP (DOBEAM, IERR)
C-----------------------------------------------------------------------
C   BMSHP fits an eliptical Gaussian to the dirty beam (DOBEAM true) and
C   checks that the peak of the beam is 1.0.  If peak of beam is too
C   narrow to fit a default circular Gaussian is used.  A grid of up to
C   5 X 11 points is used for the fit; only points within the half power
C   points are used.  To avoid degenerate cases some of the allowed
C   points are ignored.  Solution is by least squares to a linearized
C   gaussian.  Also sets Steer/Dewdney/Ito parameters.
C   Beam R   map assumed in GRD file.
C   Input:  The beam map centered at (NX/2+1,NY/2+1)
C     XSPACE    R   Grid spacing in RA (deg)
C     YSPACE    R   Grid spacing in Dec (deg.)
C     MAPROT    R   Coordinate rotation.
C   Output:
C     GAUSA     R    Major axis size (FWHP in sec).
C     GAUSB     R    Minor axis size (FWHP in sec).
C     GAUSC     R    Position angle of major axis (degrees).
C     STGAIN    R    SDI loop gain
C     STFRAC    R    SDI clip level (fraction of peak)
C-----------------------------------------------------------------------
      LOGICAL   MAP, EXCL, WAIT, DOBEAM, FA
      INTEGER   FIND, BIND, WIN(4), IERR, TRY, I, IFLIP, IJK, ILAST,
     *   IROW, J, K, L, IBEMC, IBEMH
      REAL      X(3,3), Y(3), P(3), BMAP(1), DX, DY, XFACT,
     *   BABS, BDIF, BPP, BP1, BP2, BPSUM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (BMAP(1), BUFF1(1))
      DATA MAP, EXCL, WAIT / .TRUE., 2*.TRUE./
      DATA FA /.FALSE./
C-----------------------------------------------------------------------
      NOFIT = FA
      TRY = 0
      IFLIP = 1
      XFACT = ABS (XSPACE) * 3600.
      BP1 = 0.0
      BP2 = 0.0
      BPSUM = 0.0
C                                       Must be odd!
      NBEMVL = 11
      IBEMH = NBEMVL / 2
C                                       Open beam file.
      CALL ZOPEN (LUNGD1, FIND, GRDVOL, GRDFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Zero work arrays.
      DO 20 I = 1,3
         Y(I) = 0.0
         DO 10 J = 1,3
            X(I,J) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Set window,only need half map
      WIN(1) = NX / 2 - IBEMH + 1
      WIN(2) = NY / 2 + 1
      WIN(3) = NX / 2 + IBEMH + 1
      WIN(4) = NY / 2 + IBEMH + 1
      CALL MINIT ('READ', LUNGD1, FIND, NX, NY, WIN, BMAP, BUFSZ1,
     *   BOGRD, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Loop through rows.
      IBEMC = (NBEMVL+1) / 2
      DO 70 I = 1,IBEMC
C                                       Read row.
         CALL MDISK ('READ', LUNGD1, FIND, BMAP, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, I
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Make sure center is 1.0
         IF (I.EQ.1) THEN
            BDIF = BMAP(BIND+IBEMH) - 1.0
            BABS = ABS (BDIF)
            IF (BABS.GT.0.01) GO TO 980
            END IF
         DO 42 J = 1,NBEMVL
            BEMVAL(J,I+IBEMH) = BMAP(BIND+J-1)
            IF (I.NE.1) BEMVAL(NBEMVL+1-J,IBEMC+1-I) = BMAP(BIND+J-1)
 42         CONTINUE
C                                       Loop down row doing alternate
C                                       halves. go only to first
C                                       decending 0.35 from center.
         DO 65 IJK = 1,2
            IFLIP = - IFLIP
            ILAST = BIND + IBEMH - IFLIP
            DO 60 J = IJK,IBEMC
               IROW = BIND + IBEMH + (J-1) * IFLIP
               IF ((BMAP(IROW).LT.0.35) .AND. (BMAP(IROW).LT.
     *            BMAP(ILAST))) GO TO 65
               IF (BMAP(IROW).GE.0.35) THEN
                  ILAST = IROW
C                                       Get 2nd highest point
                  BPP = BMAP(IROW)
                  BPSUM = BPSUM + BPP
                  IF ((BPP.GE.BP1) .OR. (BPP.GE.BP2)) THEN
                     IF (BPP.GE.BP1) THEN
                        BP2 = BP1
                        BP1 = BPP
                     ELSE
                        BP2 = BPP
                        END IF
                     END IF
C                                       Compute displacements from
C                                       center.
                  DX = IFLIP * (J-1.0) * XSPACE * 3600. / XFACT
                  DY = (1.0-I) * YSPACE * 3600. / XFACT
C                                       Compute partials WRT C1,C2,C3
                  P(1) = DX * DX
                  P(2) = DY * DY
                  P(3) = DX * DY
C                                       Sum partials into X matrix and
C                                       Y vector.
                  DO 50 K = 1,3
                     Y(K) = Y(K) - LOG (BMAP(IROW)) * P(K)
                     DO 45 L = 1,3
                        X(K,L) = X(K,L) + P(K) * P(L)
 45                     CONTINUE
 50                  CONTINUE
                  END IF
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
C                                       Set SDI parms
      IF ((STFRAC.LT.BP2) .OR. (STFRAC.GT.0.98)) STFRAC = 1.01*BP2
      IF (STFRAC.GT.0.98) STFRAC = 0.98
      IF (BPSUM.LE.0.0) BPSUM = 1.0
C      BPP = (1.0 - STFRAC) / BPSUM
      BPP = (1.0 - STFRAC)
      IF ((STGAIN.LE.0.0) .OR. (STGAIN.GT.BPP)) STGAIN = 0.9 * BPP
      IF (STGAIN.GT.GAIN) STGAIN = GAIN
C                                       Fit beam
      IF (DOBEAM) THEN
         CALL FITBM (0, X, Y, XFACT, MAPROT, GAUSA, GAUSB, GAUSC)
         NOFIT = .FALSE.
         END IF
C                                       Close
      CALL ZCLOSE (LUNGD1, FIND, IERR)
      GO TO 999
C                                       Beam center not 1.000.
 980  WRITE (MSGTXT,1980) BMAP(BIND+5)
      CALL MSGWRT (8)
      CALL ZCLOSE (LUNGD1, FIND, IERR)
      IERR = 8
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BMSHP: ERROR ',I3,' OPENING FILE ')
 1020 FORMAT ('BMSHP: ERROR ',I3,' INIT FILE ')
 1030 FORMAT ('BMSHP: READ ERROR ',I3,' ROW ',I5)
 1980 FORMAT ('BMSHP ERROR: CENTER OF BEAM = ',E12.5)
      END
      SUBROUTINE CLNHIS (NAME, VOL, CLASS, SEQ, OLD)
C-----------------------------------------------------------------------
C   CLNHIS copies the dirty map history if the CLEAN map does not
C   already have a history file.  Then the inputs to STCLN with
C   the default values are added to the history file.
C   INPUTS:
C     NAME(3)   C*12 Names of maps
C                      1 = Dirty map
C                      2 = Beam map
C                      3 = Clean map
C     VOL(3)      I    Vol. numbers of the maps.
C     CLASS(3)    C*6  Classes of the map.
C     SEQ(3)      I    Sequence numbers of the maps.
C     OLD         L    T => Clean map pre-exists & restart > 0
C   From common /MAPHDR/
C     CATBLK(256) I    Catalog header block for the CLEAN map.
C   From COMMON /CLN/
C     NX          I    Number of cell in the map in RA.
C     NY          I    Number of cells in the map in dec.
C     BLC(7)      I    Specifies plane.
C     CLNLIM      I    Max number of iterations.(NITER)
C     CLNSTR      I    Numb. of previous CLEAN comp. to use.
C     GAIN        R    CLEAN loop gain.
C     FMIN        R    Min. CLEAN residual.
C     GAUSA       R    Major axis (FWHM) of CLEAN restoring beam
C     GAUSB       R    Minor axis (FWHM) of CLEAN restoring beam
C     GAUSC       R    Position angle of CLEAN restoring beam.
C     NBOXS       I    Number of boxes for CLEAN.
C     WINM(4,50)  I    Windows of the CLEAN boxes.
C     BUFF1(*)    R    Work buffer
C     BUFF2(*)    R    Work buffer
C     LUNCL1      I    LUN for CLEAN history file
C     LUNDRT      I    LUN for dirty history file
C     CLNVER      I    CLEAN comp. file version number.
C   From COMMON /CFILES/
C     FCNO(10)    I    Catalog slot numbers of catalogd maps.
C                      2 = Dirty map
C                      3 = Clean map
C   OUTPUT:
C     The input parameters are written on the history file and the
C     log file.
C-----------------------------------------------------------------------
      CHARACTER NAME(3)*12, CLASS(3)*6, HILINE*72, XCLS*8,
     *   XNAM*12
      INTEGER   VOL(3), SEQ(3), CATBLK(256), IERR, I, J, I4T
      LOGICAL   T, OLD, LOLD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /MAPHDR/ CATBLK
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Open old file: no copy old HI
      IF (OLD) THEN
         CALL HIOPEN (LUNCL1, VOL(3), CCNO, BUFF2, IERR)
         IF (IERR.EQ.5) GO TO 20
         IF (IERR.EQ.0) THEN
            CALL ZDATE (BUFF1(1))
            CALL ZTIME (BUFF1(4))
            CALL TIMDAT (BUFF1(4), BUFF1(1), XCLS, XNAM)
            WRITE (MSGTXT,1000) TSKNAM, RLSNAM, XNAM, XCLS
            HILINE = MSGTXT
            CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 50
            LOLD = .TRUE.
            GO TO 30
         ELSE
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 60
            END IF
         END IF
C                                       Copy/open history files.
 20   LOLD = .FALSE.
      CALL KEYPCP (VOL(1), FCNO(2), VOL(3), CCNO, 0, ' ', IERR)
      CALL HISCOP (LUNDRT, LUNCL1, VOL(1), VOL(3), FCNO(2), CCNO,
     *   CATBLK, BUFF1, BUFF2, IERR)
C                                       Check if old file doesn't exist
      IF (IERR.GT.2) THEN
         IF (IERR.EQ.3) GO TO 50
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (6)
         GO TO 60
         END IF
C                                       Add to history file.
C                                       Dirty map name.
 30   WRITE (MSGTXT,2000) NAME(1), CLASS(1), SEQ(1), VOL(1)
      CALL MSGWRT (3)
      CALL HENCO1 (TSKNAM, NAME(1), CLASS(1), SEQ(1), VOL(1),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Dirty beam name.
      WRITE (MSGTXT,2001) NAME(2), CLASS(2), SEQ(2), VOL(2)
      CALL MSGWRT (3)
      CALL HENCO2 (TSKNAM, NAME(2), CLASS(2), SEQ(2), VOL(2),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Clean map name.
      WRITE (MSGTXT,2002) NAME(3), CLASS(3), SEQ(3), VOL(3)
      CALL MSGWRT (1)
      CALL HENCOO (TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2015) TSKNAM, BLC
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       CC file version number.
      WRITE (HILINE,2012) TSKNAM, CLNVER
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Maximum number of components.
      WRITE (HILINE,2003) TSKNAM, CLNLIM
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Start component.
      WRITE (HILINE,2004) TSKNAM, CLNSTR
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Loop gain.
      WRITE (HILINE,2005) TSKNAM, GAIN
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Speed up factor
      WRITE (HILINE,2006) TSKNAM, SPEXP
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Flux minimum.
      WRITE (HILINE,2007) TSKNAM, FMIN
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Prussian hat size
      WRITE (HILINE,2013) TSKNAM, PHAT
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       CLEAN restoring beam
      WRITE (HILINE,2008) TSKNAM, GAUSA, GAUSB, GAUSC
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
      IF (NOFIT) THEN
         WRITE (HILINE,2014) TSKNAM
         CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       Number of boxes.
      WRITE (HILINE,2009) TSKNAM, NBOXS
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Windows.
      DO 40 I = 1,NBOXS
         WRITE (HILINE,2010) TSKNAM, I, (WINM(J,I), J = 1,4)
         MSGTXT = HILINE(7:)
         CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
         CALL MSGWRT (1)
         IF (IERR.NE.0) GO TO 50
 40      CONTINUE
C                                       Min beam patch
      I = MINPCH + 1
      WRITE (HILINE,2011) TSKNAM, I
      MSGTXT = HILINE(7:)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Max pixels searched
      WRITE (HILINE,2016) TSKNAM, MAXPIX
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       SDI parms
      IF (STPNTS.GT.0.5) THEN
         I4T = STPNTS + 0.01
         WRITE (HILINE,2040) TSKNAM, I4T
         MSGTXT = HILINE(7:)
         CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
         CALL MSGWRT (1)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2041) TSKNAM, STFRAC
         MSGTXT = HILINE(7:)
         CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
         CALL MSGWRT (1)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2042) TSKNAM, STGAIN
         MSGTXT = HILINE(7:)
         CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
         CALL MSGWRT (1)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       Error has occured.
 50   IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (6)
         END IF
C                                       Close history files.
 60   CALL HICLOS (LUNCL1, T, BUFF2, IERR)
C                                       copy tables
      IF (.NOT.LOLD) THEN
         CALL ALLTAB (1, 'CC', LUNDRT, LUNCL1, VOL(1), VOL(3), FCNO(2),
     *      CCNO, CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'ERROR COPYING TABLE FILES'
            CALL MSGWRT (6)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Restart ',
     *   A12,2X,A8)
 1010 FORMAT ('UNABLE TO OPEN OLD CLEAN-MAP HISTORY FILE')
 1020 FORMAT ('CLNHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT ('CLNHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT ('DIRTY MAP =''',A12,''' . ''',A6,''' . ',2I4)
 2001 FORMAT ('BEAM MAP = ''',A12,''' . ''',A6,''' . ',2I4)
 2002 FORMAT ('CLEAN MAP =''',A12,''' . ''',A6,''' . ',2I4)
 2003 FORMAT (A6,'NITER = ',I8,' /Max. no. CLEAN iterations')
 2004 FORMAT (A6,'BITER = ',I8,' /No. of previous CLEAN comp.')
 2005 FORMAT (A6,'GAIN = ',F7.4,' /CLEAN loop gain factor')
 2006 FORMAT (A6,'FACTOR =',F5.2,' /Speed-up factor')
 2007 FORMAT (A6,'FLUX = ',E12.5,' /Min. CLEAN comp. (JY)')
 2008 FORMAT (A6,'BMAJS=',F11.6,' BMINS=',F11.6,' BPAD=',F8.3,
     *   ' /Clean beam')
 2009 FORMAT (A6,'NBOXES = ',I3,' /No. CLEAN windows')
 2010 FORMAT (A6,'BOX(',I2,') = ',I5,', ',I5,', ',I5,', ',I5,
     *   ' /CLEAN window')
 2011 FORMAT (A6,'MINPATCH = ',I4,' /Min allowed beam patch radius')
 2012 FORMAT (A6,'OUTVER = ',I4,' /CLEAN comp. file version no.')
 2013 FORMAT (A6,'PHAT = ',E12.5,' /Prussian hat size')
 2014 FORMAT (A6,'/ Beam fit failed')
 2015 FORMAT (A6,'BLC =',7I5,' / Plane CLEANed')
 2016 FORMAT (A6,'MAXPIXEL=',I10,' / Max pixels searched in cycle')
 2040 FORMAT (A6,'STPOINTS=',I10,' / Pixels to start SDI')
 2041 FORMAT (A6,'STFRACT =',F6.4,4X,' / SDI clip level')
 2042 FORMAT (A6,'STGAIN  =',F6.4,4X,' / SDI loop gain')
      END
      SUBROUTINE DECIDE (DOSTER)
C-----------------------------------------------------------------------
C   DECIDE determines a beam patch and limiting map value (PATCH and
C   MAPLIM) which will optimize the use of the array processor.
C   Inputs:
C       DOSTER L     IN => SDI already started, OUT => do SDI?
C       BMHIS  I   = max. fractional abs. of sidelobes external to
C                    beam patch I + 1.  See HISTOB for details.
C       RESHIS I   = Histogram of abs residual map values.
C                    See HISTOR for details.
C       NUMBIN I   = Number of levels for fraction of the peak.
C       MAXRES I   = Max. abs. residual map value .
C       MAXPCH I   = Maximum beam patch size allowed.
C       MINPCH I   = Minimum beam patch size allowed.
C       ICENX  I   = Offset in X of beam from NX/2+1
C       ICENY  I   = Offset in Y of beam from NY/2+2
C   Output:
C       PATCH  I   = Beam patch size (max. distance from the center)
C       MAPLIM R   = Minimum abs. map level to be considered. (scaled
C                    I)   Only values GREATER than MAPLIM should be
C                    used.
C-----------------------------------------------------------------------
      CHARACTER PREFIX*5
      REAL      TEMP, XFLUX
      INTEGER   I, IPMAP, JLEV, K, KPAT, MLOOP, IT1, IT2, I2TMP, CORE,
     *   I4TMP, MAXRES, LMXPCH
      LOGICAL   LERR, DOSTER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA MAXRES /20050/
C-----------------------------------------------------------------------
C                                       Do SDI now?
      IF (STPNTS.GT.0.5) THEN
         IF (.NOT.DOSTER) THEN
            TEMP = RESMAX
            IF (TEMP.LE.0.0) TEMP = 1.0
            TEMP = MAX (ABS(TVREMN), ABS(TVREMX)) / TEMP
            IF (TEMP.LE.0.0) TEMP = 1.0
            IT1 = STFRAC * NUMBIN * TEMP + 1.5
            WRITE (MSGTXT,1000) RESHIS(IT1), TEMP
            CALL MSGWRT (3)
            DOSTER = RESHIS(IT1)+0.1.GE.STPNTS
            END IF
         IF (DOSTER) GO TO 999
         END IF
C                                       Determine max PATCH for NBOXS=1
      IPMAP = WINM(3,1) - WINM(1,1) + 1
      IF (IPMAP.LT.(WINM(4,1)-WINM(2,1)+1)) IPMAP = WINM(4,1)-
     *   WINM(2,1) + 1
      IF (IPMAP.LT.0) IPMAP = -IPMAP + 2
C                                       Loop thru beam patch sizes to fi
C                                       the largest which, with necessar
C                                       map points will fit in the AP.
      IT1 = NX / 2 - ABS (ICENX)
      IT2 = NY / 2 - ABS (ICENY)
      LMXPCH = MIN (MAXPCH, BMHSIZ, IT1, IT2) - 1
C                                       set maximum # pixels searched
      IF ((MAXPIX.LT.1000) .OR. (MAXPIX.GT.500000)) MAXPIX = MAXRES
      IT1 = (APSIZ - BMOFFS - (LMXPCH+1) * (2*LMXPCH + 1)) / 3 - 10
      MAXPIX = MIN (MAXPIX, IT1)
C                                       Minimum MINPCH is 3.
      IT1 = NX / 2
      IT2 = NY / 2
      MINPCH = MIN (MINPCH, IT1, IT2)
      IT1 = LMXPCH - 5
      MINPCH = MIN (MINPCH, IT1)
      MINPCH = MAX (MINPCH, 3)
      MLOOP = LMXPCH - MINPCH + 1
      DO 50 I = 1,MLOOP
         KPAT = LMXPCH - I + 1
C                                       Ignore beam patches with no info
         IF (BMHIS(KPAT).GT.0) THEN
C                                       Compute amount of AP core for be
            CORE = KPAT + 1
            CORE = CORE * (2 * CORE - 1)
C                                       Use value in BMHIS as an index
C                                       for RESHIS.
            JLEV = BMHIS(KPAT)
            IF (JLEV.GE.2) THEN
C                                       Make sure some data to be loaded
               I2TMP = JLEV
               DO 30 K = 2,I2TMP
                  IF (RESHIS(JLEV).GT.0) GO TO 35
                  JLEV = JLEV - 1
 30               CONTINUE
               END IF
C                                       Compute total AP core for this
C                                       PATCH - MAPLIM combination.
 35         I4TMP = RESHIS(JLEV)
            CORE = CORE + 3 * I4TMP
C                                       See if this will fit and if so
C                                       jump out of the loop.
            IF ((CORE.LT.APSIZ-BMOFFS) .AND. (RESHIS(JLEV).LE.MAXPIX))
     *         GO TO 60
            END IF
 50      CONTINUE
C                                       If the program gets here, no bea
C                                       patch - map limit would fit.
C                                       Use minimum beam patch  and
C                                       determine which map limit will f
      DO 55 I = 1,NUMBIN
         JLEV = I
         CORE = RESHIS(JLEV)
         I4TMP = MINPCH + 1
         CORE = (I4TMP * (2 * I4TMP - 1)) + 3 * CORE
         IF ((CORE.LT.APSIZ-BMOFFS) .AND. (RESHIS(JLEV).LE.MAXPIX))
     *      GO TO 60
   55    CONTINUE
C                                      If program gets here there is a
C                                      serious problem. e.g. residual ma
C                                      has a constant (or nearly) value
      WRITE (MSGTXT,1055)
      CALL MSGWRT (8)
C                                      Use JLEV =NUMBIN -1/2 and hope fo
C                                      best.
      MAPLIM = RESMAX * (NUMBIN-0.5) / NUMBIN
      PATCH = MINPCH
      GO TO 990
C                                      An acceptable solution was found.
 60   MAPLIM = (RESMAX * (JLEV - 0.5)) / (NUMBIN - 1.0)
      PATCH = KPAT + 1
C                                       See if everything will fit.
      CORE = BMOFFS + 3.0 * RESHIS(1) + REAL(PATCH)*(2.*PATCH-1)+0.5
      IF ((NBOXS.EQ.1) .AND. (PATCH.GE.IPMAP) .AND. (CORE.LE.APSIZ)
     *   .AND. (RESHIS(1).LE.MAXPIX)) MAPLIM = -1.0E-10 * RESMAX
C                                       Display the results.
 990  XFLUX = MAPLIM
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1990) XFLUX, PREFIX
      CALL MSGWRT (2)
      WRITE (MSGTXT,1991) PATCH
      CALL MSGWRT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I10,' Pixels above the SDI clip level',F8.4)
 1055 FORMAT ('DECIDE: CANNOT OBTAIN BEAM PATCH - MAP LIMIT PROPERLY')
 1990 FORMAT ('In-AP CLEAN uses residual points brighter than',F10.3,
     *   1X,A5,'Jy')
 1991 FORMAT ('and a beam half-width of',I5,' cells')
      END
      SUBROUTINE DISPTV (TVPASS)
C-----------------------------------------------------------------------
C   DISPTV displays the current residual map on the TV, showing inner
C   portion only if that's all that will fit.
C   Inputs:  TVPASS  I     code: 0,1 => clear screen, else don't
C                                0,3 => don't question the user about
C                                       quitting
C   Output:  TVPASS  I     code: 32700 => user wants to quit cleaning
C-----------------------------------------------------------------------
      CHARACTER  PREFIX*5
      INTEGER   TVPASS, JROW(1), WIN(4), MY, FIND, BIND, IERR, ICH,
     *   CATBLK(256), S2H(256), IQ, IB, I, MX2, MX, INC(2), MV
      INTEGER   IWIN(4), IY
      REAL      XN(4), XBUFF(1), CATR(256), TD, RPOS(2)
      REAL      XFLUX, TVLMAX, TVLMIN, ARG
      HOLLERITH CATH(256)
      LOGICAL   MAP, EXCL, WAIT, LERR, F
      DOUBLE PRECISION CATD(128)
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVD.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (JROW(1), BUFF2(1)),    (BUFF1(1), XBUFF(1))
      EQUIVALENCE (S2H, CATR, CATD, CATH)
      DATA MAP, EXCL, WAIT / .TRUE., 2*.TRUE./
C-----------------------------------------------------------------------
      ICH = 1
      CALL TVOPEN (BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
      IF (TVPASS.LE.1) THEN
         CALL TVSET (ICH, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 998
            END IF
         END IF
      IF (TVFMAX.LE.TVFMIN) THEN
         TVFMAX = TVREMX
         TVFMIN = TVREMN
         END IF
      IF (TVREMX.GT.TVFMAX) TVFMAX = TVREMX
      IF (TVREMN.LT.TVFMIN) TVFMIN = TVREMN
      TVLMAX = TVFMAX - TVFMIN
      IF (0.1*TVLMAX.GT.TVREMX-TVREMN) THEN
         ARG = 0.1 * TVFMIN
         TVFMIN = MIN (ARG, TVREMN)
         ARG = TVFMIN + 0.1 * TVLMAX
         TVFMAX = MAX (ARG, TVREMX)
         TVLMAX = TVFMAX - TVFMIN
         END IF
      XN(1) = TVFMIN
      XN(2) = TVFMAX
      XN(3) = (MAXINT - 0.02) / TVLMAX
      XN(4) = 0.51 - TVFMIN * XN(3)
      CALL QPUT (XN, 0, 4, 2)
C                                       Write scaling factor
      XFLUX = TVLMAX
      CALL METSCA (XFLUX, PREFIX, LERR)
      TVLMIN = TVFMIN * XFLUX / TVLMAX
      TVLMAX = TVFMAX * XFLUX / TVLMAX
      WRITE (MSGTXT,1020) TVLMIN, TVLMAX, PREFIX
      CALL MSGWRT (1)
C                                       center on center of image
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      INC(1) = NX / MAXXTV(1) + 1
      INC(2) = NY / MAXXTV(2) + 1
      INC(1) = MAX (INC(1), INC(2))
      INC(2) = MAX (INC(1), INC(2))
      DO 70 I = 1,2
         WIN(I+2) = WIN(I+2) - MOD (WIN(I+2)-WIN(I), INC(I))
         IWIN(I) = (MAXXTV(I) - (WIN(I+2)- WIN(I))/INC(I) + 1)/2
         IF (IWIN(I).LT.1) THEN
            IWIN(I) = 1
            WIN(I)  = (WIN(I+2) + WIN(I) - MAXXTV(I)*INC(I) + 1)/2
            IWIN(I+2) = MAXXTV(I)
            WIN(I+2) = WIN(I) + (IWIN(I+2) - IWIN(I)) * INC(I)
         ELSE
            IWIN(I+2) = IWIN(I) + (WIN(I+2) - WIN(I)) / INC(I)
            IF (IWIN(I+2).GT.MAXXTV(I)) THEN
               IWIN(I+2) = MAXXTV(I)
               WIN(I+2) = WIN(I) + (IWIN(I+2) - IWIN(I)) * INC(I)
               END IF
            END IF
 70      CONTINUE
      WRITE (MSGTXT,1070) XN(1), XN(2), INC(1)
      CALL MSGWRT (2)
C                                       Prepare to read map.
      CALL ZOPEN (LUNRES, FIND, RESVOL, RESFIL, MAP, EXCL, WAIT, IERR)
      CALL MINIT ('READ', LUNRES, FIND, NX, NY, WIN, XBUFF, BUFSZ1,
     *   BORES, IERR)
      MX = WIN(3) - WIN(1) + 1
      MY = WIN(4) - WIN(2) + 1
      MV = IWIN(3) - IWIN(1) + 1
      MX2 = MV
C                                       loop, passing map to TV
      IY = IWIN(2) - 1
      DO 100 I = 1,MY
         CALL MDISK ('READ', LUNRES, FIND, XBUFF, BIND, IERR)
         IF (IERR.NE.0) GO TO 110
         IF (MOD(I-1, INC(2)).EQ.0) THEN
            CALL QPUTIN (XBUFF(BIND), INC(1), 4, 1, MV, 2)
            CALL QWD
            CALL QVCLIP (4, 1, 0, 1, 4, 1, MV)
            CALL QVSMSA (4, 1, 2, 3, 4, 1, MV)
            CALL QVFIX (4, 1, 4, 1, MV)
            CALL QWR
            CALL QGET (JROW, 4, MV, 1)
            CALL QWD
C                                       Send row to TV
            IY = IY + 1
            CALL YIMGIO ('WRIT', ICH, IWIN, IY, 0, MX2, JROW, IERR)
            IF (IERR.NE.0) GO TO 110
            END IF
 100     CONTINUE
 110  CALL ZCLOSE (LUNRES, FIND, IERR)
C                                       Release the AP
      CALL QRLSE
C                                       Image catalog
      CALL COPY (256, CATBLK, S2H)
      S2H(IIVOL) = 0
      S2H(IICNO) = 0
      CALL FILL (5, 1, S2H(IIDEP))
      CALL COPY (4, IWIN, S2H(IICOR))
      CALL COPY (4, WIN, S2H(IIWIN))
      CALL CHR2H (2, '  ', 1, CATH(IITRA))
      CATR(IRRAN) = TVFMIN
      CATR(IRRAN+1) = TVFMAX
      CATR(KRDMN) = TVREMN
      CATR(KRDMX) = TVREMX
      CALL YCWRIT (ICH, IWIN, S2H, XBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110)
         CALL MSGWRT (6)
         END IF
C                                       Ask user to quit?
      IF ((TVPASS.EQ.1) .OR. (TVPASS.EQ.2)) THEN
         MSGTXT = 'Hit button D within 15 seconds to stop cleaning now'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons A, B, or C to continue sooner'
         CALL MSGWRT (1)
         RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2.0
         RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2.0
         TD = 0.2
         CALL YCURSE ('ONNN', F, F, RPOS, IQ, IB, IERR)
         IF (IERR.NE.0) GO TO 998
         DO 130 I = 1,75
            CALL ZDELAY (TD, IERR)
            CALL YCURSE ('READ', F, F, RPOS, IQ, IB, IERR)
C                                       Wants to quit
            IF (IB.GT.7) THEN
               TVPASS = 32700
               MSGTXT = 'TV Button D hit: have done enough I guess'
               CALL MSGWRT (3)
               GO TO 150
            ELSE IF ((IB.GT.0) .OR. (IERR.NE.0)) THEN
               GO TO 135
               END IF
 130        CONTINUE
 135     MSGTXT = 'Continuing'
         CALL MSGWRT (1)
         GO TO 150
         END IF
C                                       Off cursor
 150  CALL YCURSE ('OFFF', F, F, RPOS, IQ, IB, IERR)
 998  CALL TVCLOS (BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CAN''T OPEN TV IER=',I6)
 1010 FORMAT ('IMCLEAR ERROR =',I6)
 1020 FORMAT ('TVDISP: Display range =',2F8.3,1X,A5,'Jy')
 1070 FORMAT ('Loading TV from',1PE10.3,' to',1PE10.3,' every',I2,
     *   ' pixel')
 1110 FORMAT ('CAN''T UPDATE IMAGE CATALOG  IER=',I6)
      END
      SUBROUTINE FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   FILES reads and floats the  beam and creates the
C   clean map and components files. The beam is placed in the
C   GRD file and the location of the cataloged files are
C   left in the DRT and BEM files.
C   BEAM and Dirty maps in catalog are marked READ and Clean map
C   is marked WRITE.
C   Input:
C     USID     I    User number.
C     NAME(3)  C*12 Names of maps
C                     1 = Dirty map
C                     2 = Beam map
C                     3 = CLEAN map
C     CLASS(3) C*6  Classes of the maps
C     VOL(3)   I    Volumns of the maps.
C   Output:
C     ICENX    I    Offset in X of BEAM center from NX/2+1
C     ICENY    I    Offset in Y of BEAM center from NY/2+1
C     OLD      L    TRUE if old map file being replaced.
C     WINM(4,NBOXS)  I    Adjusted dirty map window.
C     NBOXS    I    Number of boxes in the map to CLEAN.
C   Commons:
C     CATBLK(256) in /MAPHDR/ a preliminary header for the clean map
C        is returned.
C-----------------------------------------------------------------------
      CHARACTER NAME(3)*12, CLASS(3)*6,  STAT*4, MTYPE*2
      INTEGER   ISIZE, LSIZE, IEQUIV
      INTEGER   USID, CATBLK(256), CNO, VOL(3), SEQ(3),
     *   OLDIM, OLDNAX(7), I, IBOX, IER, IERR, LREC, IDEP(5), IROUND,
     *   CORN(7), JWIN(4), NOSCR, NPIX(2), IX, IY
      LOGICAL   OLD
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), OLDCRV(7)
      REAL      CATR(256), XMAX, XMIN, XCEN, YCEN, REQUIV
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (IEQUIV, REQUIV)
      DATA CORN, JWIN / 7*1, 4*0/
C-----------------------------------------------------------------------
      NCFILE = 0
C                                       Read dirty beam.
C                                       Get catalog slot for BEAM.
      CNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', VOL(2), CNO, NAME(2), CLASS(2), SEQ(2),
     *    MTYPE, USID, STAT, BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(2), CLASS(2), SEQ(2), VOL(2),
     *      USID
         GO TO 990
         END IF
C                                       Copy CATBLK and mark beam READ
      CALL CATIO ('READ', VOL(2), CNO, CATBLK, 'READ', BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(2)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 0
C                                       Determine NX and NY
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      CALL POWER2 (NX, IX)
      CALL POWER2 (NY, IY)
C                                       Invalid (NX,NY)
      IF ((NX.NE.IX) .OR. (NX.LT.32) .OR. (NX.GT.MAXIMG) .OR.
     *   (NY.NE.IY) .OR. (NY.LT.32) .OR. (NY.GT.MAXIMG)) GO TO 980
C                                       Determine beam center pixel
      CALL COPY (5, BLC(3), IDEP)
C                                       See if beam cube
      IF (CATBLK(KINAX+2).EQ.1) IDEP(1) = 1
      CALL PEAKFN (LUNGD1, VOL(2), CNO, IDEP, CATBLK, BUFF1, BUFSZ1,
     *   XCEN, YCEN, IERR)
      IF (IERR.NE.0) GO TO 999
      XCEN = XCEN - NX/2 - 1
      YCEN = YCEN - NY/2 - 1
      ICENX = IROUND (XCEN)
      ICENY = IROUND (YCEN)
C                                       Create scratch files.
C                                       Use output as residual file
C                                       Grid file
      NSCR = 0
      NPIX(1) = NX
      NPIX(2) = NY + 1
      CALL MAPSIZ (2, NPIX, ISIZE)
      NPIX(1) = 2 * NPIX(1)
      NPIX(2) = NY/2 + 1
      CALL MAPSIZ (2, NPIX, LSIZE)
      IF (LSIZE.GT.ISIZE) ISIZE = LSIZE
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      GRDVOL = SCRVOL(NSCR)
      CNOGRD = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', GRDVOL, CNOGRD, 1, GRDFIL, IERR)
      NOSCR = NSCR
C                                       Work file
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      WRKVOL = SCRVOL(NSCR)
      CNOWRK = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', WRKVOL, CNOWRK, 1, WRKFIL, IERR)
C                                       Weight file
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      WTVOL = SCRVOL(NSCR)
      CNOWT = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', WTVOL, CNOWT, 1, WTFIL, IERR)
C                                       Read beam map to GRID
      CALL COPY (5, IDEP, CORN(3))
      CALL PLNGET (VOL(2), CNO, CORN, JWIN, ICENX, ICENY, NOSCR, NX,
     *   NY, BUFF1, BUFF2, BUFSZ1, BUFSZ2, LUNGD1, LUNDRT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
C                                       Get and store info on BEAM.
C                                       Store information on BEAM file
C                                       Use GRD file for temp R   BEAM.
      CALL ZPHFIL ('MA', VOL(2), CNO, 1, BEMFIL, IER)
      BEMVOL = VOL(2)
      SEQ(2) = CATBLK(KIIMS)
C                                       Set BLKOF for plane.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, BOBEM, IERR)
      BOBEM = BOBEM + 1
C                                       Loop through the boxes.
C                                       Find max,min X, Y
      MAXX = -30000
      MINX = 30000
      MAXY = -30000
      MINY = 30000
      DO 70 IBOX = 1,NBOXS
C                                       Make sure window no larger
C                                       than map size.
         IF (WINM(3,IBOX).GT.NX) WINM(3,IBOX) = NX
         IF (WINM(4,IBOX).GT.NY) WINM(4,IBOX) = NY
C                                       If default use center 1/4 of map
         IF (WINM(1,IBOX).LE.0) WINM(1,IBOX) = NX / 4 + 1
         IF (WINM(2,IBOX).LE.0) WINM(2,IBOX) = NY / 4 + 1
         IF (WINM(3,IBOX).LE.0) WINM(3,IBOX) = 3*NX/4 - 1
         IF (WINM(4,IBOX).LE.0) WINM(4,IBOX) = 3*NY/4 - 1
         MAXX = MAX (MAXX, WINM(1,IBOX), WINM(3,IBOX))
         MINX = MIN (MINX, WINM(1,IBOX), WINM(3,IBOX))
         MAXY = MAX (MAXY, WINM(2,IBOX), WINM(4,IBOX))
         MINY = MIN (MINY, WINM(2,IBOX), WINM(4,IBOX))
 70      CONTINUE
      IF (MINX.LE.0) MINX = 1
      IF (MAXX.LE.0) MAXX = NX
      IF (MINY.LE.0) MINY = 1
      IF (MAXY.LE.0) MAXY = NY
C                                       Create convolution scratch files
      NPIX(1) = MAXX - MINX + 1
      NPIX(2) = MAXY - MINY + 1
      CALL MAPSIZ (2, NPIX, ISIZE)
      DO 80 I = 1,3
         CALL SCREAT (ISIZE, BUFF3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
            END IF
         CONVOL(I) = SCRVOL(NSCR)
         CNOCON(I) = SCRCNO(NSCR)
         CALL ZPHFIL ('SC', CONVOL(I), CNOCON(I), 1, CONFIL(I), IERR)
 80      CONTINUE
      BOCONV = 1
C                                       Get catalog slot number for
C                                       the dirty map and it's CATBLK.
      CNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', VOL(1), CNO, NAME(1), CLASS(1), SEQ(1),
     *    MTYPE, USID, STAT, BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(2), CLASS(2), SEQ(1), VOL(1),
     *      USID
         GO TO 990
         END IF
C                                       Get CATBLK for dirty map.
C                                       Leave file marked READ.
      CALL CATIO ('READ', VOL(1), CNO, CATBLK, 'READ', BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1090) IERR
         GO TO 990
         END IF
C                                       Mark map READ in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(1)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 0
C                                       Warn if "Clean"
      IF (CATBLK(KINIT).GT.0) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
         END IF
C                                       Get dirty scale,offset,max,min.
C                                       Set info into COMMON.
      XMAX = CATR(KRDMX)
      XMIN = CATR(KRDMN)
      RESMAX = MAX (ABS(XMAX) , ABS(XMIN))
      CALL ZPHFIL ('MA', VOL(1), CNO, 1, DRTFIL, IER)
      DRTVOL = VOL(1)
C                                       Set BLKOF for plane.
      IDEP(1) = BLC(3)
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, BODRT, IERR)
      BODRT = BODRT + 1
C                                        RA spacing.
      XSPACE = CATR(KRCIC)
C                                        Dec spacing.
      YSPACE = CATR(KRCIC+1)
C                                        Map rotation.
      CALL ROTFND (CATR, MAPROT, IER)
C                                       Make sure NX,NY same as
C                                       for the beam.
      IF ((NX.NE.CATBLK(KINAX)) .OR. (NY.NE.CATBLK(KINAX+1))) THEN
         IERR = 1
         WRITE (MSGTXT,1105)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1106) NX,NY,CATBLK(KINAX),CATBLK(KINAX+1)
         GO TO 990
         END IF
C                                       Put NAME in CATBLK
      CALL CHR2H (12, NAME(3), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASS(3), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQ(3)
C                                       Create output CLEAN map file.
      CALL MCREAT (VOL(3), CNO, BUFF1, IERR)
      OLD = .FALSE.
      IF (IERR.EQ.0) GO TO 300
         OLD = .TRUE.
         IF (IERR.EQ.2) GO TO 200
            WRITE (MSGTXT,1190) IERR
            GO TO 990
C                                       Existing file: check it out
 200     CONTINUE
            OLDIM = CATBLK(KIDIM)
            DO 205 I = 1,7
               OLDNAX(I) = CATBLK(KINAX+I-1)
               OLDCRV(I) = CATD(KDCRV+I-1)
 205           CONTINUE
            CALL CATIO ('READ', VOL(3), CNO, CATBLK, 'REST', BUFF3,
     *         IERR)
            IF (OLDIM.NE.CATBLK(KIDIM)) GO TO 215
            DO 210 I = 1,OLDIM
               IF (OLDNAX(I).NE.CATBLK(KINAX+I-1)) GO TO 215
               IF (ABS (OLDCRV(I)-CATD(KDCRV+I-1)) .GT. 0.05 *
     *            ABS (CATR(KRCIC+I-1))) GO TO 215
 210           CONTINUE
C                                       It matches, mark WRITE
            CALL CATIO ('WRIT', VOL(3), CNO, CATBLK, 'WRIT', BUFF3,
     *         IERR)
            IF (IERR.EQ.0) GO TO 300
               WRITE (MSGTXT,1210) IERR
               GO TO 990
C                                       Not same file !!!
 215        CONTINUE
               IERR = 8
               WRITE (MSGTXT,1215)
               GO TO 990
C                                       Actual seq #
 300  SEQ(3) = CATBLK(KIIMS)
C                                       Add to /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(3)
      CLNVOL = VOL(3)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
      IF (OLD) FRW(NCFILE) = 1
      CCNO = CNO
      LREC = 3
C                                       Use output file as resid. file
      RESVOL = VOL(3)
      CNORES = CNO
      CALL ZPHFIL ('MA', RESVOL, CNORES, 1, RESFIL, IERR)
      BORES = BODRT
C                                       If cube, override CLNVER w/
C                                       plane number.
      IF (CATBLK(KINAX+2).GT.1) CLNVER = BLC(3)
C                                       Check limit of 46655
      IF (CLNVER.GT.46655) THEN
         IERR = 5
         WRITE (MSGTXT,1301) CLNVER
         GO TO 990
         END IF
C                                       Create/get ver # for CC file.
      IF (CLNVER.EQ.0) CALL FNDEXT ('CC', CATBLK, CLNVER)
      CALL CCINI (LUNCL2, LREC, CLNVOL, FCNO(3), CLNVER, CATBLK,
     *   BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1300) IERR
         GO TO 990
         END IF
C                                       Check no. CC comps.
      REQUIV = BUFF3(5)
      IF (CLNSTR.GT.IEQUIV) CLNSTR = IEQUIV
      IEQUIV = CLNSTR
      BUFF3(5) = REQUIV
C                                      Close.
      CALL TABIO ('CLOS', 0, LREC, BUFF1, BUFF3, IERR)
      GO TO 999
C
 980  WRITE (MSGTXT,1980) NX, NY
      IERR = 8
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILES: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I4)
 1010 FORMAT ('FILES: CANNOT COPY BEAM CATBLK, ERROR',I3)
 1020 FORMAT ('FILES: ERROR',I3,' CREATING GRID SCRATCH FILE')
 1025 FORMAT ('FILES: ERROR',I3,' CREATING WORK SCRATCH FILE')
 1030 FORMAT ('FILES: ERROR',I3,' CREATING WEIGHT SCRATCH FILE')
 1035 FORMAT ('FILES: CANNOT COPY DIRTY BEAM, ERROR ',I3)
 1070 FORMAT ('FILES: ERROR',I3,' CREATING S.D. CONV SCRATCH FILE')
 1090 FORMAT ('FILES: CANNOT COPY MAP CATBLK, ERROR',I3)
 1100 FORMAT ('WARNING: MAY BE CLEANING A CLEAN MAP')
 1105 FORMAT ('FILES: UNEQUAL DIMENSIONS IN DIRTY AND BEAM MAPS')
 1106 FORMAT ('       BEAM =',2I5,' DIRTY =',2I5)
 1190 FORMAT ('FILES: COULD NOT CREATE CLEAN MAP FILE, ERROR ',I3)
 1210 FORMAT ('FILES: CANNOT UPDATE CLEAN CATBLK, ERROR',I3)
 1215 FORMAT ('OLD CLEAN MAP NOT COMPATIBLE WITH DIRTY MAP')
 1300 FORMAT ('FILES: UNABLE TO CREATE CLEAN COMP. FILE, ERROR',I3)
 1301 FORMAT ('COMPONENT FILE NO. TOO LARGE ',I3,' >46655')
 1980 FORMAT ('MAP DIMENSIONS:',2I5,' NOT INTEGER POWERS OF 2')
      END
      SUBROUTINE GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IERR)
C-----------------------------------------------------------------------
C   GETIN gets the input parameters for the program from AIPS
C  See Prologue for STCLN for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, NAME(3)*12, CLASS(3)*6, DEFCLS*6, MTYPE*2
      INTEGER   USID, SEQ(3), VOL(3), ITV
      INTEGER   NPARMS, IERR, CATBLK(256), IND, ITEMP,
     *   CNO, I, IK, IL, J, IROUND, IT1
      HOLLERITH XNAM1(3), XNAM2(3), XNAM3(3), XCLAS1(2), XCLAS2(2),
     *   XCLAS3(2)
      REAL   XSEQ1, XSEQ2, XSEQ3, XVOL1, XVOL2, XVOL3, XBLC(7),
     *   XG, XF, XNI, XBI, XBMJ, XBMN, XBPA, XNB, XBOX(4,50), XDOTV,
     *   XFCTR, XBAD(10), XVER, XINC, XMAXPX, XPHAT, XSTPNT, XSTFRA,
     *   XSTGAI
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XVOL1, XBLC, XNAM2, XCLAS2,
     *   XSEQ2, XVOL2, XNAM3, XCLAS3, XSEQ3, XVOL3, XVER, XG, XF,
     *   XPHAT, XSTPNT, XSTFRA, XSTGAI, XNI, XBI, XBMJ, XBMN, XBPA, XNB,
     *   XBOX, XFCTR, XINC, XMAXPX, XDOTV, XBAD
      DATA PRGNAM /'SDCLN '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize common parameters.
C                                        global areas
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL HIINIT (3)
C                                        non-standard messages
C                                        STCLN common areas
      BMOFFS = 2 * BMHSIZ + 4
      BOBEM = 1
      BORES = 1
      BOWT = 1
      BOGRD = 1
      BODRT = 1
      BOWRK = 1
      LUNBEM = 16
      LUNRES = 17
      LUNRS1 = 19
      LUNWT  = 20
      LUNGD1 = 19
      LUNGD2 = 21
      LUNDRT = 22
      LUNWRK = 23
      LUNCL1 = 26
      LUNCL2 = 27
      CLNLIM = 200
      CLNSTR = 0
      CLNLST = 0
      FLUX = 0.0
      FMIN = 0.000001
      MINPCH = 127
      GAIN = 0.10
      NUMBIN = HBUFSZ - 10
      ITEMP = 2  * LBUFSZ
      BUFSZ1 = ITEMP
      BUFSZ2 = ITEMP
      BUFSZ3 = ITEMP
      BPS = NBPS
      TVFMAX = 0.0
      TVFMIN = 0.0
C                                       Get AIPS adverbs.
      NPARMS = 255
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Convert characters
      CALL H2CHR (12, 1, XNAM1, NAME(1))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (12, 1, XNAM2, NAME(2))
      CALL H2CHR (6, 1, XCLAS2, CLASS(2))
      CALL H2CHR (12, 1, XNAM3, NAME(3))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
C                                       Find desired plane no.
      BLC(1) = 1
      BLC(2) = 1
      DO 20 I = 3,7
         BLC(I) = MAX (1.0, XBLC(I)) + 0.5
 20      CONTINUE
C                                       get actual dirty map name
      VOL(1) = IROUND (XVOL1)
      SEQ(1) = IROUND (XSEQ1)
      USID = NLUSER
      CNO = 1
      MTYPE = 'MA'
      CALL MAPOPN ('READ', VOL, NAME(1), CLASS(1), SEQ, MTYPE, USID,
     *   LUNBEM, IND, CNO, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL MAPCLS ('READ', VOL, CNO, LUNBEM, IND, CATBLK, F, BUFF1,
     *   IERR)
C                                       Default name for Beam and CLEAN
C                                       maps is Dirty map name.
      IF (NAME(2).EQ.' ') NAME(2) = NAME(1)
C                                       Copy CLASSes or default.
      IF (CLASS(2).EQ.' ') THEN
         CLASS(2) = 'IBM001'
         IF (CLASS(1)(1:1).EQ.'R') CLASS(2)(1:1) = 'R'
         IF (CLASS(1)(1:1).EQ.'L') CLASS(2)(1:1) = 'L'
         END IF
C                                       Use appropriate default for
C                                       polarization type.
      DEFCLS = 'ICL001'
      IF (CLASS(1)(1:1).EQ.'Q') DEFCLS(1:1) = 'Q'
      IF (CLASS(1)(1:1).EQ.'U') DEFCLS(1:1) = 'U'
      IF (CLASS(1)(1:1).EQ.'V') DEFCLS(1:1) = 'V'
      IF (CLASS(1)(1:1).EQ.'R') DEFCLS(1:1) = 'R'
      IF (CLASS(1)(1:1).EQ.'L') DEFCLS(1:1) = 'L'
C                                       Get volumn numbers
      VOL(2) = IROUND (XVOL2)
      VOL(3) = IROUND (XVOL3)
C                                       Get sequence numbers
      SEQ(2) = IROUND (XSEQ2)
      SEQ(3) = IROUND (XSEQ3)
      IF (SEQ(2).LE.0) SEQ(2) = SEQ(1)
      CALL MAKOUT (NAME, CLASS, SEQ, DEFCLS, NAME(3), CLASS(3), SEQ(3))
C                                       Get CC file version no.
      CLNVER = IROUND (XVER)
C                                       Get CLEANing information
      IF (XG.GT.0.0) GAIN = XG
      FMIN = XF
      PHAT = XPHAT
      IF (XNI.GT.0.5) CLNLIM = XNI + 0.5
      IF (XNI.LT.0.5) CLNLIM = XNI - 0.5
      CLNSTR = XBI + 0.5
      GAUSA = XBMJ
      GAUSB = XBMN
      GAUSC = XBPA
      STPNTS = XSTPNT
      STFRAC = XSTFRA
      STGAIN = XSTGAI
C                                       set maximum # pixels searched
      MAXPIX = XMAXPX + 0.5
      IF ((MAXPIX.LT.1000) .OR. (MAXPIX.GT.500000)) MAXPIX = 20050
C                                       Check number of comps
C                                       subtracted from dirty map.
      PRECLN = 0
      IF (CATBLK(KINIT).GT.0) PRECLN = CATBLK(KINIT)
      NBOXS = IROUND (XNB)

      NBOXS = MIN (NBOXS, 50)
      CALL FILL (200, 0, WINM)
      IF (NBOXS.GT.0) THEN
         IL = 1
         DO 60 I = 1,NBOXS
            IK = 0
            DO 50 J = 1,4
               WINM(J,IL) = IROUND (XBOX(J,I))
               IF (WINM(J,IL).GT.0) IK = IK + 1
 50            CONTINUE
            IF ((I.EQ.1) .OR. (IK.GT.0)) IL = IL + 1
 60         CONTINUE
         IT1 = IL - 1
         NBOXS = MIN (IT1, NBOXS)
         END IF
      NBOXS = MAX (NBOXS, 1)
      SPEXP  = XFCTR
      IF (XINC.GT.3.0) MINPCH = XINC - 0.9
      ITV = 0
      IF (XDOTV.GT.0.0) ITV = 1
C                                       Get bad disks.
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 70      CONTINUE
C                                       Check if TV allowed
      IF (NTVDEV.LE.0) ITV = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
      END
      SUBROUTINE GRIDER (WFIRST, IERR)
C-----------------------------------------------------------------------
C   GRIDER Fourier transforms the CLEAN components and multiplies
C   times the negative of the  weighting function so that when
C   retransformed and added to the Dirty map the current residual map
C   results.  Results are accumulated in a file GRID so that only
C   new components need be transformed.
C   Output is to file WRK and then the names and addresses of the GRID
C   and WRK files are switched.
C    Input:
C     CLNSTR I   = highest number component to transform
C     CLNLST I   = first CLEAN component to transform.
C     WFIRST L   = .TRUE. iff this is first write on grid file.
C     CLEAN components from file CLNFIL.
C   Output:
C     Transform of additional CLEAN components times the weighting fn
C     added to the file GRDFIL.
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, IERR, IRET,  WIN(4),
     *   BIND1, BIND2, BIND3, NLOAD, I, JLIM
      INTEGER   NUMCLN, FIRST, MAXCMP, JNUM, NAPGRD, IAPSIZ,
     *   NAPWTS, NAPCMP, NAPCMS, APBUF, NUMBER, TWONY, ONENY,
     *   WRK1, WRK2
      LOGICAL   MAP, EXCL, WAIT, WFIRST, F, DOSUM
      REAL      WBUFF(1), GBUFF(1), OBUFF(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
C      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (WBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      EQUIVALENCE (OBUFF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,2*.TRUE./
      DATA F /.FALSE./
      DATA APBUF /1/
C-----------------------------------------------------------------------
      IAPSIZ = 1024 * PSAPNW
      DOSUM = WFIRST
      IF (DOSUM) FLUX = 0.0
C                                       Determine Area assignments in AP
      ONENY = NY
      TWONY = 2 * NY
      NAPGRD = 100
      NAPWTS = NAPGRD + TWONY + 1
      NAPCMP = NAPWTS + TWONY + 1
      NAPCMS = NAPCMP + TWONY + 1
C                                       Determine AP buffer size.
      NLOAD = NAPCMS - 3
C                                       Determine maximum number of comp
C                                       per pass.
      MAXCMP = (IAPSIZ - NAPCMS) / 5.0
C                                       Determine number of components
C                                       to be CLEANed.
      NUMCLN = CLNSTR - CLNLST
C                                       Begin component loop.
 10   CONTINUE
         FIRST = CLNLST + 1
         NUMBER = MAXCMP
         IF (NUMCLN.LT.NUMBER) NUMBER = NUMCLN
C                                       Load NUMBER components this pass
         JNUM = NUMBER
         CALL CMPCRM (NAPCMS, APBUF, FIRST, NUMBER, NLOAD, DOSUM, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Open weights file and init.
         CALL ZOPEN (LUNWT, FIND1, WTVOL, WTFIL, MAP, EXCL, WAIT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NY * 2
         WIN(4) = NX / 2 + 1
         CALL MINIT ('READ', LUNWT, FIND1, WIN(3), WIN(4), WIN, WBUFF,
     *      BUFSZ1, BOWT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            GO TO 990
            END IF
C                                       Open and INIT GRID file for read
         CALL ZOPEN (LUNGD1, FIND2, GRDVOL, GRDFIL, MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            GO TO 990
            END IF
         WIN(3) = NY * 2
         WIN(4) = NX / 2 + 1
         CALL MINIT ('READ', LUNGD1, FIND2, WIN(3), WIN(4), WIN, GBUFF,
     *      BUFSZ2, BOGRD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Open and INIT WRK file for write
         CALL ZOPEN (LUNWRK, FIND3, WRKVOL, WRKFIL, MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            GO TO 990
            END IF
         CALL MINIT ('WRIT', LUNWRK, FIND3, WIN(3), WIN(4), WIN, OBUFF,
     *      BUFSZ3, BOWRK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Clear NAPGRD.
         CALL QVCLR (NAPGRD, 1, TWONY)
C                                       Begin loop thru map.
         JLIM = NX / 2 + 1
         DO 150 I = 1,JLIM
C                                       Do direct transform in X.
            CALL QVCLR (NAPCMP, 1, TWONY)
            CALL QDIRAD (NAPCMS, 5, NAPCMP, NUMBER)
C                                       Read weights.
            CALL MDISK ('READ', LUNWT, FIND1, WBUFF, BIND1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR,I
               GO TO 990
               END IF
C                                       Do FFT in Y.
            CALL QWR
            CALL QCFFT (NAPCMP, ONENY, 1)
C                                       Read row from GRID file.
C                                       except on first pass
            IF (.NOT.WFIRST) THEN
               CALL MDISK ('READ', LUNGD1, FIND2, GBUFF, BIND2, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1100) IERR,I
                  GO TO 990
                  END IF
               END IF
C                                       Load weights into AP.
            CALL QWR
            CALL QPUT (WBUFF(BIND1), NAPWTS, TWONY, 2)
            CALL QWD
C                                       Load row from GRID file if nec.
            IF (.NOT.WFIRST) CALL QPUT (GBUFF(BIND2), NAPGRD, TWONY, 2)
            CALL QWD
C                                       Multiply by weights.
            CALL QCVMUL (NAPCMP, 2, NAPWTS, 2, NAPCMP, 2, ONENY, 1)
            CALL QWR
C                                       Subtract from previous GRID file
            CALL QVSUB (NAPCMP, 1, NAPGRD, 1, NAPCMP, 1, TWONY)
C                                       Write file back to disk.
            CALL MDISK ('WRIT', LUNWRK, FIND3, OBUFF, BIND3, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1090) IERR, I
               GO TO 990
               END IF
C                                       Read results back out.
            CALL QWR
            CALL QGET (OBUFF(BIND3), NAPCMP, TWONY, 2)
            CALL QWD
C                                       Rotate DFT for next pass.
            WRK1 = NAPCMS + 1
            WRK2 = NAPCMS + 3
            CALL QCVMUL (WRK1, 5, WRK2, 5, WRK1, 5, NUMBER, 1)
            CALL QWR
 150        CONTINUE
C                                       End of this pass, close files.
         CALL ZCLOSE (LUNWT, FIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1150) IERR
            CALL MSGWRT (8)
            END IF
         CALL ZCLOSE (LUNGD1, FIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1160) IERR
            CALL MSGWRT (8)
            END IF
C                                       Finish writing GRID file.
         CALL MDISK ('FINI', LUNWRK, FIND3, OBUFF, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1170) IERR
            GO TO 990
            END IF
         CALL ZCLOSE (LUNWRK, FIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1160) IERR
            CALL MSGWRT (8)
            END IF
         CLNLST = CLNLST + JNUM
         NUMCLN = NUMCLN - JNUM
C                                       Switch WRK and GRID files.
         CALL FSWTCH (GRDFIL, WRKFIL, GRDVOL, WRKVOL, CNOGRD, CNOWRK,
     *      BOGRD, BOWRK)
C                                       Check for more CLEAN components.
         WFIRST = F
C                                       Call AP roller before next pass
         IF (CLNLST.LT.CLNSTR) CALL QROLL (0, BUFF1, BUFSZ1, IRET)
         IF (CLNLST.LT.CLNSTR) GO TO 10
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('GRIDER: ERROR ',I3,' OPENING FILE ')
 1020 FORMAT ('GRIDER: ERROR ',I3,' INIT WEIGHT FILE')
 1030 FORMAT ('GRIDER: ERROR ',I3,' OPENING GRID FILE ')
 1050 FORMAT ('GRIDER: ERROR ',I3,' INIT GRID FILE')
 1090 FORMAT ('GRIDER: WRITE ERROR ',I3,' GRID ROW ',I6)
 1080 FORMAT ('GRIDER: ERROR ',I3,' READ WEIGHT ROW ',I6)
 1100 FORMAT ('GRIDER: ERROR ',I3,' READ GRID ROW ',I6)
 1150 FORMAT ('GRIDER: ERROR ',I3,' CLOSING WEIGHT FILE')
 1160 FORMAT ('GRIDER: ERROR ',I3,' CLOSING GRID FILE')
 1170 FORMAT ('GRIDER: FINISH ERROR ',I3,' WRITING GRID FILE ')
      END
      SUBROUTINE HISTOB (IERR)
C-----------------------------------------------------------------------
C   HISTOB determines the largest exterior sidelobe as a function of
C   the beam patch size. Beam assumed in GRD file.
C    Input:
C        LUMGRD I   = Logical unit number for the beam file.
C        GRDFIL I   = the name of the beam map file.
C        MAXPCH I   = maximum beam patch size to be considered.
C        NX,NY  I   = X and Y dimensions of the full beam map.(cells).
C        NUMBIN I   = Number of levels considered for the fraction
C                     of the peak.
C        BOGRD I   = Blok offset for beam map.
C    Output:
C        BMHIS  I   = An array whose elements have values between 1
C                     and NUMBIN + 1 indicating the maximum abs.
C                     exterior sidelobe for a beam patch whose size
C                     corresponds to the array index.
C                     e.g. for a beam patch of half size I the maximum
C                     fractional absolute sidelobe level exterior to the
C                     beam patch is BMHIS(I) / NUMBIN  .
C-----------------------------------------------------------------------
      INTEGER   FIND, BIND, IERR, IPCH, IY, I, ITEMP, J, JJ, LIMIT,
     *   MAXMUM, NXHAF, NYHAF, IT1, IT2, LMXPCH
      LOGICAL   MAP, EXCL, WAIT
      REAL      XBUFF(1), BMBUF(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (XBUFF(1), BUFF1(1)),      (BMBUF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /3*.TRUE./
C-----------------------------------------------------------------------
C                                       Search entire beam map.
C                                       Center is at (NX/2+1,NY/2+1).
      NYHAF = NY / 2 + 1
      NXHAF = NX / 2 + 1
      MAXMUM = MAX (NYHAF, NXHAF)
C                                      Open beam file.
      CALL ZOPEN (LUNGD1, FIND, GRDVOL, GRDFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Check to be sure MAXPCH is permi
      IT1 = NX / 2 - 1
      IT2 = NY / 2 - 1
      LMXPCH = MIN (MAXPCH, IT1, IT2, BMHSIZ)
C                                       Set window for histogram.
      WINB(1) = 1
      WINB(2) = 1
      WINB(3) = NX
      WINB(4) = NYHAF
C                                      Initialize beam map.
       CALL MINIT ('READ', LUNGD1, FIND, NX, NY, WINB, XBUFF, BUFSZ1,
     *    BOGRD, IERR)
       IF (IERR.NE.0) THEN
          WRITE (MSGTXT,1010) IERR
          GO TO 990
          END IF
C                                      Initialize BMHIS to zero.
       DO 20 I = 1,MAXMUM
         BMBUF(I) = 0
 20      CONTINUE
C                                      Loop thru map looking for max.
C                                      abs. sidelobe level on each beam
C                                      patch boundary.
      DO 40 I = 1,NYHAF
C                                      Read row of the map.
         CALL MDISK ('READ', LUNGD1, FIND, XBUFF, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR,I
            GO TO 990
            END IF
         IY = NYHAF - I - 1
C                                      Loop down the row.
         DO 30 J = 1,NX
            JJ = BIND + J - 1
C                                      Compute min. beam patch including
C                                      this pixel. Adjust the index so t
C                                      order in array gives the largest
C                                      EXTERIOR sidelobe.
            ITEMP = J - NXHAF
            ITEMP = ABS (ITEMP) - 1
            IPCH = MAX (IY, ITEMP, 1)
C                                      Replace current value of
C                                      BMBUF(IPCH) if abs. value of
C                                      this pixel is larger.
            BMBUF(IPCH) = MAX (BMBUF(IPCH), ABS(XBUFF(JJ)))
 30         CONTINUE
 40      CONTINUE
C                                      Close beam file.
      CALL ZCLOSE (LUNGD1, FIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         END IF
C                                      Make sure BMBUF for each beam
C                                      patch is.GE.that for larger
C                                      beam patches.
      LIMIT = MAXMUM - 1
      DO 50 I = 1,LIMIT
         J = MAXMUM - I + 1
         IF (BMBUF(J-1).LT.BMBUF(J)) BMBUF(J-1) = BMBUF(J)
C                                      Check that value. le. 1.0
         IF (ABS (BMBUF(J-1)).GT.1.05) THEN
            WRITE (MSGTXT,1045) BMBUF(J-1)
            IERR = 6
            GO TO 990
            END IF
 50      CONTINUE
C                                      Scale BMHIS to range (1,NUMBIN+1)
      DO 60 I = 1,LMXPCH
         BMHIS(I) = BMBUF(I) * NUMBIN + 1.5
 60      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HISTOB: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('HISTOB: ERROR',I3,' INIT FILE ')
 1020 FORMAT ('HISTOB: READ ERROR',I3,' ROW ',I5)
 1040 FORMAT ('HISTOB: ERROR',I3,' CLOSING FILE ')
 1045 FORMAT ('HISTOB: BAD BEAM VALUE =',1PE10.3)
      END
      SUBROUTINE MAPPAK (IERR)
C-----------------------------------------------------------------------
C   MAPPAK takes points from the residual map greater than MAPLIM
C   and places them with their addresses into the array processor.
C    Input :
C      LUNRES I   = Logical unit number for the residual map file.
C      RESFIL I   = Name of the residual map file.
C      MAPLIM R   = Limiting map value .
C      BORES  I   = Blok offset for the residual map.
C      WINM   I   = Windows for the map.
C      NBOXS  I   = Number of boxes in CLEAN map
C      APSIZ  I   = Size of the Array Processor in words.
C   Output:
C     Residual map points greater than MAPLIM and their addresses
C     are placed in the array processor.
C-----------------------------------------------------------------------
      INTEGER   FIND, BIND, IERR, I, IBOX, IPTR, J, JJ, MY,
     *   WIN(4), LOX, HIX, LOY, HIY
      INTEGER   JPTR, JPTRP1, LPTR
      LOGICAL   MAP, EXCL, WAIT
      REAL      V(1), X(1), Y(1), XBUFF(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (XBUFF(1), BUFF1(1)),      (X(1), BUFF2(1)),
     *   (Y(1), BUFF2(4097)),        (V(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /3*.TRUE./
C-----------------------------------------------------------------------
C                                      Open residual file.
      CALL ZOPEN (LUNRES, FIND, RESVOL, RESFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IPTR = 0
      PTRJ = BMOFFS
      WIN(1) = 1
      WIN(2) = MINY
      WIN(3) = NX
      WIN(4) = MAXY
      MY = HIY - LOY + 1
C                                      Initialize residual map.
      CALL MINIT ('READ', LUNRES, FIND, NX, NY, WIN, XBUFF, BUFSZ1,
     *   BORES, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Loop thru map looking for points
C                                       greater than MAPLIM.
      IPTR = 0
      DO 80 I = MINY,MAXY
C                                      Read row of the map.
         CALL MDISK ('READ', LUNRES, FIND, XBUFF, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, I
            GO TO 990
            END IF
C                                       Loop over boxes
         DO 70 IBOX = 1,NBOXS
            LOY = MIN (WINM(2,IBOX), WINM(4,IBOX))
            HIY = MAX (WINM(2,IBOX), WINM(4,IBOX))
            IF (HIY.LE.0) HIY = NY
            IF ((I.GE.LOY) .AND. (I.LE.HIY)) THEN
               LOX = MIN (WINM(1,IBOX), WINM(3,IBOX))
               HIX = MAX (WINM(1,IBOX), WINM(3,IBOX))
C                                      Loop down the row.
               DO 60 J = LOX, HIX
                  JJ = BIND + J - 1
                  IF (ABS (XBUFF(JJ)).GT.MAPLIM) THEN
                     IPTR = IPTR + 1
                     V(IPTR) = XBUFF(JJ)
                     X(IPTR) = J
                     Y(IPTR) = I
C                                      Check to see if buffer full.
                     IF (IPTR.GE.256) THEN
C                                       Check if AP full.
                        IF (PTRJ+IPTR*3.GE.APSIZ) GO TO 90
                        JPTR = PTRJ
                        JPTRP1 = JPTR + 2
                        CALL QWR
                        CALL QPUT (V, 2, 256, 2)
                        CALL QWD
                        CALL QVMOV (2, 1, JPTRP1, 3, 256)
                        CALL QWR
                        CALL QPUT (Y, 2, 256, 2)
                        JPTRP1 = JPTR + 1
                        CALL QWD
                        CALL QVMOV (2, 1, JPTRP1, 3, 256)
                        CALL QWR
                        CALL QPUT (X, 2, 256, 2)
                        CALL QWD
                        CALL QVMOV (2, 1, JPTR, 3, 256)
                        IPTR = 0
                        PTRJ = PTRJ + 3 * 256
                        END IF
                     END IF
 60               CONTINUE
               END IF
 70         CONTINUE
 80      CONTINUE
      GO TO 100
C                                      If program gets here AP filled.
 90   WRITE (MSGTXT,1090)
      CALL MSGWRT (8)
C                                      Write last buffer into the AP.
C                                      Check if AP full.
 100  IF ((IPTR.GT.0) .AND. (PTRJ+IPTR*3.LT.APSIZ)) THEN
         LPTR = IPTR
         JPTR = PTRJ
         JPTRP1 = PTRJ + 2
         CALL QWR
         CALL QPUT (V, 2, LPTR, 2)
         CALL QWD
         CALL QVMOV (2, 1, JPTRP1, 3, LPTR)
         CALL QWR
         CALL QPUT (Y, 2, LPTR, 2)
         JPTRP1 = JPTR + 1
         CALL QWD
         CALL QVMOV (2, 1, JPTRP1, 3, LPTR)
         CALL QWR
         CALL QPUT (X, 2, LPTR, 2)
         CALL QWD
         CALL QVMOV (2, 1, JPTR, 3, LPTR)
         PTRJ = PTRJ + 3 * IPTR
         END IF
C                                      Close residual file.
      CALL ZCLOSE (LUNRES, FIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (8)
         END IF
C                                       Set and print number of points.
      PTRK = (PTRJ - BMOFFS) / 3
      WRITE (MSGTXT,1120) PTRK
      CALL MSGWRT (2)
C                                       Call AP roller
      JPTR = PTRJ
      CALL QROLL (JPTR, BUFF1, BUFSZ1, IERR)
C                                       Check that some data loaded
      IF (PTRK.GT.0) GO TO 999
         IERR = 1
         WRITE (MSGTXT,1121)
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAPPAK: ERROR',I3,' OPENING FILE ')
 1020 FORMAT ('MAPPAK: ERROR',I3,' INIT FILE ')
 1030 FORMAT ('MAPPAK: READ ERROR',I3,' ROW ',I5)
 1090 FORMAT ('MAPPAK: NOT ENOUGH ROOM FOR ALL MAP POINTS ')
 1110 FORMAT ('MAPPAK: ERROR',I3,' CLOSING FILE ')
 1120 FORMAT ('MAPPAK:',I8,' Residual map points loaded into AP')
 1121 FORMAT ('NO RESIDUAL POINTS LOADED INTO AP, I QUIT')
      END
      SUBROUTINE RESTOR (IERR)
C-----------------------------------------------------------------------
C   RESTOR restores CLEAN components to GRID file.  This is done by
C   transforming the CLEAN components, multiplying by the appropriate
C   Gaussian function and adding to the GRID file.
C   Output is to file WRK and then the names and addresses of the WRK
C   and GRID files are switched.
C   Input:
C     CLNSTR I   = highest number component to transform
C     CLEAN components from file CLNFIL.
C   Output:
C     Transform of additional CLEAN components times the restoring fn
C     added to the file GRDFIL.
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, BIND2, BIND3, IERR, WIN(4), NLOAD, I, JLIM
      INTEGER   FIRST, NUMCLN, MAXCMP, JNUM, IAPSIZ, WRK1, WRK2, NAPGAU,
     *   NAPEX1, NAPEXP, NAPRES, NAPGRD, NAPCMP, NAPCMS, NUMBER, APBUF,
     *   ONENY, TWONY
      LOGICAL   MAP, EXCL, WAIT, F
      REAL      TEMP, GBUFF(1), OBUFF(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
C      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (OBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      DATA APBUF / 1 /
      DATA EXCL, WAIT, MAP, F /3*.TRUE., .FALSE./
C-----------------------------------------------------------------------
      IAPSIZ = 1024 * PSAPNW
C                                       Determine Area assignments in AP
      ONENY = NY
      TWONY = 2 * NY
      NAPRES = 100
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
      NAPEXP = NAPRES + WRK1
      NAPGRD = NAPEXP + WRK1
      NAPCMP = NAPGRD + WRK2
      NAPGAU = NAPCMP + WRK2
      NAPEX1 = NAPGAU + WRK1
      NAPCMS = NAPEX1 + WRK1
C                                       Determine maximum number of comp
C                                       per pass.
      MAXCMP = (IAPSIZ - NAPCMS) / 5.0
      NUMCLN = CLNSTR
      CLNLST = 1
      APBUF = 2
C                                       Determine size of AP buffer for
      NLOAD = NAPGAU - 3
C                                       Prepare work arrays in AP.
      CALL INGAUS
C                                       Roll AP if necessary
      CALL QROLL (NAPCMS, BUFF3, BUFSZ3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Begin component loop.
 10   CONTINUE
         NUMBER = MAXCMP
         IF (NUMCLN.LT.MAXCMP) NUMBER = NUMCLN
C                                       Load CLEAN components this pass.
         FIRST = CLNLST
         JNUM = NUMBER
         CALL CMPCRM (NAPCMS, APBUF, FIRST, NUMBER, NLOAD, F, IERR)
         IF (IERR.NE.0) GO TO 999
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NY * 2
         WIN(4) = NX / 2 + 1
C                                       Open and INIT GRID file for read
         CALL ZOPEN (LUNGD1, FIND2, GRDVOL, GRDFIL, MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         CALL MINIT ('READ', LUNGD1, FIND2, WIN(3), WIN(4), WIN, GBUFF,
     *      BUFSZ2, BOGRD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Open and INIT GRID file for writ
         CALL ZOPEN (LUNWRK, FIND1, WRKVOL, WRKFIL, MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         CALL MINIT ('WRIT', LUNWRK, FIND1, WIN(3), WIN(4), WIN, OBUFF,
     *      BUFSZ1, BOWRK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Prepare NAPEXP using weight left
C                                       in AP loc 0 by INGAUS.
         CALL QVCLR (NAPEXP, 1, ONENY)
         CALL QVSADD (NAPEXP, 1, 0, NAPEXP, 1, ONENY)
         CALL QWR
C                                       Begin loop thru map.
         JLIM = NX / 2 + 1
         DO 150 I = 1,JLIM
C                                        Generate restoring factors.
            TEMP = (-0.5 * GAUSCC * (I - 1.0) ** 2 )
            CALL QPUT (TEMP , 1, 1, 2)
            CALL QWAIT
            CALL QVSADD (NAPGAU, 1, 1, NAPRES, 1, ONENY)
            CALL QVADD (NAPRES, 1, NAPEXP, 1, NAPRES, 1, ONENY)
            CALL QVEXP (NAPRES, 1, NAPRES, 1, ONENY)
C                                       Read row from GRID file.
            CALL MDISK ('READ', LUNGD1, FIND2, GBUFF, BIND2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR,I
               GO TO 990
               END IF
C                                       Write file back to disk.
            CALL MDISK ('WRIT', LUNWRK, FIND1, OBUFF, BIND3, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1090) IERR, I
               GO TO 990
               END IF
C                                       Prepare NAPEXP for next pass.
            CALL QVADD (NAPEXP, 1, NAPEX1, 1, NAPEXP, 1, ONENY)
            CALL QWR
C                                       Clear NAPCMP.
            CALL QVCLR (NAPCMP, 1, TWONY)
C                                       Do direct transform in X.
            CALL QDIRAD (NAPCMS, 5, NAPCMP, NUMBER)
            CALL QWR
C                                       Do FFT in Y.
            CALL QCFFT (NAPCMP, ONENY, 1)
            CALL QWR
C                                       Load row from GRID file.
            CALL QPUT (GBUFF(BIND2), NAPGRD, TWONY, 2)
            CALL QWD
C                                       Multiply by restoring function.
            CALL QCRVMU (NAPCMP, 2, NAPRES, 1, NAPCMP, 2, ONENY)
            CALL QWR
C                                       Add to GRID file.
            CALL QVADD (NAPCMP, 1, NAPGRD, 1, NAPCMP, 1, TWONY)
C                                       Read results back out.
            CALL QWR
            CALL QGET (OBUFF(BIND3), NAPCMP, TWONY, 2)
            CALL QWD
C                                       Rotate DFT for next pass.
            WRK1 = NAPCMS + 1
            WRK2 = NAPCMS + 3
            CALL QCVMUL (WRK1, 5, WRK2, 5, WRK1, 5, NUMBER,
     *         1)
            CALL QWR
 150        CONTINUE
C                                       Finish writing GRID file.
         CALL MDISK ('FINI', LUNWRK, FIND1, OBUFF, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR,JLIM
            GO TO 990
            END IF
C                                       End of this pass, close files.
         CALL ZCLOSE (LUNGD1, FIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1160) IERR
            CALL MSGWRT (8)
            END IF
         CALL ZCLOSE (LUNWRK, FIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1160) IERR
            CALL MSGWRT (8)
            END IF
         CLNLST = CLNLST + JNUM
         NUMCLN = NUMCLN - JNUM
C                                       Switch WRK and GRID files.
         CALL FSWTCH (GRDFIL, WRKFIL, GRDVOL, WRKVOL, CNOGRD, CNOWRK,
     *      BOGRD, BOWRK)
C                                       Roll AP if necessary
         IF (CLNLST.LE.CLNSTR) THEN
            CALL QROLL (NAPCMS, BUFF3, BUFSZ3, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Check for more CLEAN components.
         IF (CLNLST.LE.CLNSTR) GO TO 10
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RESTOR: ERROR ',I3,' OPENING GRID FILE ')
 1050 FORMAT ('RESTOR: ERROR ',I3,' INIT GRID FILE')
 1090 FORMAT ('RESTOR: WRITE ERROR ',I3,' GRID ROW ',I6)
 1080 FORMAT ('RESTOR: ERROR ',I3,' READ GRID ROW ',I6)
 1160 FORMAT ('RESTOR: ERROR ',I3,' CLOSING GRID FILE')
      END
      SUBROUTINE CMPCRM (APLO, APBUF, FIRST, NUMBER, NLOAD, LFIRST,
     *   IERR)
C-----------------------------------------------------------------------
C   CMPCRM loads CLEAN components into the Array Processor
C   in preparation for transformation to the data plane.
C   If CMPCRM starts from the first component, the flux density
C   of the CLEAN components is summed.
C   The signs of the cell offsets are adjusted for flips made to
C   the map.
C    Input:
C     APLO   I   = AP start location for the component array.
C     APBUF  I   = AP start location for the buffer.
C     FIRST  I   = First CLEAN component to be loaded.
C     NUMBER I   = Number of CLEAN components to be loaded.
C     NLOAD  I   = AP buffer size.
C     LFIRST L   = .TRUE. iff CLEAN components are to be summed.
C    Output:
C     NUMBER  I     Number of clean components loaded.
C     CLEAN components loaded into the AP in blocks of 5 words
C     arrainged as follows:
C         0 = Y (integer form)
C         1 = FLUX * cos(UX)
C         2 = FLUX * sin(UX)
C         3,4 = cos, sin(X)
C-----------------------------------------------------------------------
      CHARACTER KEYS(4)*8
      INTEGER   FIRST, NUMBER
      INTEGER   IBUFF(512), CATBLK(256), IERR, NREC, NLOAD,
     *   LIMIT, NCOUNT, I, LREC,
     *   NKEY, KOLS(4), RAKOL, DECKOL, FLXKOL, TYPKOL
      INTEGER   APLOC, APLOC1, APLOC2, APLOC3, APLOC4, APBUF, APLO,
     *   NNCNT, JJCNT
      INTEGER   IPOINT, ICOUNT, JCOUNT
      REAL      CFLUX(1), TEMP(6), TWOPIX, RAC(1), DECC(1),
     *   RECORD(7), CATR(256), XCEN, YCEN
      LOGICAL   LFIRST, FA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL)
      EQUIVALENCE (BUFF1(1), IBUFF(1))
      EQUIVALENCE (BUFF2(1), CFLUX(1)),      (BUFF3(1), RAC(1)),
     *   (BUFF3(LBUFSZ/2+1), DECC(1)),  (CATBLK, CATR)
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ', 'TYPE OBJ'/
      DATA FA /.FALSE./
C-----------------------------------------------------------------------
C                                       Determine beam center.
      XCEN = CATR (KRCRP)
      YCEN = CATR (KRCRP+1)
      XCEN = (XCEN - NX/2 - 1) * XSPACE
      YCEN = (YCEN - NY/2 - 1) * YSPACE
C                                       Open CLEAN component file.
      LREC = 3
      NREC = 1
      NKEY = 1
      JCOUNT = 0
      CALL TABINI ('READ', 'CC', CLNVOL, FCNO(3), CLNVER, CATBLK,
     *   LUNCL2, NKEY, NREC, LREC, BUFF2, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      CALL RFILL (7, 0.0, RECORD)
      NKEY = 3
      TYPKOL = 7
      FLXKOL = 1
      RAKOL = 2
      DECKOL = 3
      IF (LREC.GT.3) NKEY = 4
      CALL FNDCOL (NKEY, KEYS, 8, FA, IBUFF, KOLS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         END IF
      APLOC = APLO
      ICOUNT = 0
      IPOINT = FIRST
C                                       Begin loading into computer buff
C                                       Make sure to load in small
C                                       enough pieces.
      LIMIT = BUFSZ1 / 8
      IF (LIMIT.GT.NUMBER) LIMIT = NUMBER
      LIMIT = MIN (LIMIT, NLOAD)
C                                       Jump to here if more passes nece
 30   CONTINUE
         NCOUNT = 0
         DO 50 I = 1,LIMIT
            CALL TABIO ('READ', 0, IPOINT, RECORD, IBUFF, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1030) IERR, IPOINT
               GO TO 990
               END IF
            IF (((LREC.LE.3) .OR. (RECORD(TYPKOL).EQ.0.0))  .AND.
     *         (IERR.EQ.0)) THEN
               NCOUNT = NCOUNT + 1
               CFLUX(NCOUNT) = RECORD(FLXKOL)
               RAC(NCOUNT) = RECORD(RAKOL) + XCEN
               DECC(NCOUNT) = RECORD(DECKOL) + YCEN
               END IF
            IPOINT = IPOINT + 1
 50         CONTINUE
         IF (NCOUNT.GT.0) THEN
            NNCNT = NCOUNT
C                                       Form AP indexes.
            APLOC1 = APLOC + 1
            APLOC2 = APLOC + 2
            APLOC3 = APLOC + 3
            APLOC4 = APLOC + 4
C                                       Load data into AP buffer APBUF.
C                                       Load flux density (FLUX*cos(ux))
            CALL QPUT (CFLUX, APBUF, NNCNT, 2)
            CALL QWD
            CALL QVMOV (APBUF, 1, APLOC1, 5, NNCNT)
C                                        Load and fix dec cell numbers.
            CALL QWR
            CALL QPUT (DECC, APBUF, NNCNT, 2)
            CALL QWD
C                                       Move values before fixing.
            CALL QVMOV (APBUF, 1, APLOC, 5, NNCNT)
            CALL QWR
C                                       Set constants in AP.
            TEMP(1) = (-NY/2.) * YSPACE
            TEMP(2) = (NY/2.0-1.0) * YSPACE
            TEMP(3) = NY * YSPACE
            TEMP(4) = 2.0 / YSPACE
            TEMP(5) = 0.0
            TEMP(6) = -0.5 * YSPACE
            CALL QPUT (TEMP, 1, 6, 2)
            CALL QWD
C                                       Put declinations in the range
C                                       (0,NY-1) and multiply by 2,fix.
C                                       Use APLOC2 for temporary use.
            CALL QVFILL (6, APLOC2, 5, NNCNT)
            CALL QVCLIP (APLOC, 5, 1, 2, APLOC, 5, NNCNT)
            CALL QLVGT (APLOC2, 5, APLOC, 5, APLOC2, 5, NNCNT)
            CALL QVSMUL (APLOC2, 5, 3, APLOC2, 5, NNCNT)
            CALL QVADD (APLOC2, 5, APLOC, 5, APLOC, 5, NNCNT)
            CALL QVSMAF (APLOC, 5, 4, 5, APLOC, 5, NNCNT)
C                                       Load and float RA cell numbers.
            CALL QWR
            CALL QPUT (RAC, APBUF, NNCNT, 2)
            CALL QWD
            CALL QVMOV (APBUF, 1, APLOC3, 5, NNCNT)
C                                       Store -2*PI/NX/XSPACE
            TWOPIX = (-6.283185/NX) / XSPACE
            CALL QWR
            CALL QPUT (TWOPIX, 1, 1, 2)
            CALL QWD
C                                       Scale RA's by -2*PI/NX/XSPACE
            CALL QVSMUL (APLOC3, 5, 1, APLOC3, 5, NNCNT)
C                                       Clear APLOC+2 (FLUX*sin(ux) ).
            CALL QVCLR (APLOC2, 5, NNCNT)
C                                       Take sine and cosine of
C                                       RA to 3 and 4.
            CALL QVSIN (APLOC3, 5, APLOC4, 5, NNCNT)
            CALL QVCOS (APLOC3, 5, APLOC3, 5, NNCNT)
            END IF
         ICOUNT = ICOUNT + LIMIT
         JCOUNT = JCOUNT + NCOUNT
C                                       If load complete close CLNFIL
C                                       and return.
         IF (ICOUNT.LT.NUMBER) THEN
C                                       Update APLOC.
            APLOC = APLOC + NNCNT * 5
            IF (NUMBER-ICOUNT.LT.LIMIT) LIMIT = NUMBER - ICOUNT
C                                       Return for another load.
            GO TO 30
            END IF
C                                       IF (LFIRST) sum fluxes.
      IF (LFIRST) THEN
         APLOC1 = APLO + 1
         JJCNT = JCOUNT
         CALL QSVE (APLOC1, 5, 1, JJCNT)
         CALL QWR
         CALL QGET (TEMP, 1, 1, 2)
         CALL QWD
         FLUX = FLUX + TEMP(1)
         END IF
C                                     Make sure CC loaded.
      IF (JCOUNT.LE.0) THEN
         WRITE (MSGTXT,1065)
         IERR = 1
         GO TO 990
         END IF
      NUMBER = JCOUNT
C                                       Close CLNFIL.
      CALL TABIO ('CLOS', 0, IPOINT, RECORD, IBUFF, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1070) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CMPCRM: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('CMPCRM: ERROR',I3,' FINDING REQUIRED CC COLUMNS')
 1030 FORMAT ('CMPCRM: READ ERROR',I3,' RECORD ',I5)
 1065 FORMAT ('CMPCRM: NO POINT CLEAN COMPONENTS FOUND')
 1070 FORMAT ('CMPCRM: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE INGAUS
C-----------------------------------------------------------------------
C   INGAUS creates the arrays in the AP necessary for restoring with
C   an eliptical gaussian.
C    Input:
C     NX,NY     I    Number of map grid cells in X and Y.
C     GAUSA     R    Major Axis (FWHP in cells)
C     GAUSB     R    Minor axis size (FWHP in cells).
C     GAUSB     R    Position angle of restoring beam (deg ).
C     XSPACE    R    RA grid spacing (deg).
C     YSPACE    R    Dec grid spacing (deg).
C     MAPROT    R    Coordinate rotation (deg).
C    Output:
C     GAUSAA     R    Coefficient of u**2
C     GAUSBB     R    Coefficient of u*v
C     GAUSCC     R    Coefficient of v**2
C     Normalization constant for the restoring Gaussian left in
C                  AP location 0.
C-----------------------------------------------------------------------
      REAL      COMP(1), WT, WT1, XNX2, XNY2, XNXNY, TEMP,
     *   AK, AM, AN, TA, TB, ARG
      INTEGER   I, JLIM, K
      INTEGER   NAPRES, NAPGAU, NAPEXP, NAPEX1, NAPGRD, NAPCMP, NAPCMS,
     *   NAPRS1, ONENY, TWONY, WRK1, WRK2
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (BUFF1(1), COMP(1))
C-----------------------------------------------------------------------
C                                       Set AP assignments.
      ONENY = NY
      TWONY = 2 * NY
      NAPRES = 100
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
      NAPEXP = NAPRES + WRK1
      NAPGRD = NAPEXP + WRK1
      NAPCMP = NAPGRD + WRK2
      NAPGAU = NAPCMP + WRK2
      NAPEX1 = NAPGAU + WRK1
      NAPCMS = NAPEX1 + WRK1
C                                       Do a little arithmetic to put
C                                       the beam parameters into useable
C                                       form. Convert to sigma, then to
C                                       coeficients of U**2, U*V, and
C                                       V**2 the last of which is return
C                                       for later use.
      TA = GAUSA * 3.1415927 / 1.1774
      TB = GAUSB * 3.1415927 / 1.1774
      AM = COS ((GAUSC + MAPROT) * 3.1415927 / 180.)
      AN = SIN ((GAUSC + MAPROT) * 3.1415927 / 180.)
      XNX2 = NX * XSPACE * 3600.
      XNY2 = NY * YSPACE * 3600.
      XNXNY = ABS (XNX2 * XNY2)
      XNX2 = XNX2 ** 2
      XNY2 = XNY2 ** 2
      GAUSAA = (TA*TA*AM*AM + TB*TB*AN*AN) / (XNY2)
      GAUSCC = (TA*TA*AN*AN + TB*TB*AM*AM) / (XNX2)
      GAUSBB = ((TB*TB-TA*TA) * AN*AM) / (XNXNY )
C                                       Create array NAPEX1.
      DO 10 I = 1,NY
         K = I - 1
         IF (I.GT.NY/2) K = K - NY
         COMP(I) = (-GAUSBB * K)
 10      CONTINUE
C                                       Load NAPEX1 into AP.
      CALL QPUT (COMP, NAPEX1, ONENY, 2)
      CALL QWD
C                                       Create array NAPGAU.
      DO 20 I = 1,NY
         K = I - 1
         IF (I.GT.NY/2) K = K - NY
         AK = K
         COMP(I) = -0.5 * GAUSAA * AK * AK
 20      CONTINUE
C                                       Load NAPGAU into AP.
      CALL QPUT (COMP, NAPGAU, ONENY, 2)
C                                       Initialize array NAPEXP.
      CALL QPUT (0.0, 0, 1, 2)
      CALL QWD
      CALL QVCLR (NAPEXP, 1, ONENY)
      CALL QVSADD (NAPEXP, 1, 0, NAPEXP, 1, ONENY)
C                                       Initialize NAPRES for summing
C                                       weights
      NAPRS1 = NAPRES - 1
      WRK1 = ONENY + 1
      CALL QVCLR (NAPRS1, 1, WRK1)
      CALL QWR
C                                       Calculate sum of the weights.
      JLIM = NX / 2
      DO 40 I = 1,JLIM
         TEMP = -0.5 * GAUSCC * (I - 1.0) ** 2
         CALL QPUT (TEMP, 1, 1, 2)
         CALL QWD
         CALL QVSADD (NAPGAU, 1, 1, NAPRES, 1, ONENY)
         CALL QVADD (NAPRES, 1, NAPEXP, 1, NAPRES, 1, ONENY)
         CALL QVEXP (NAPRES, 1, NAPRES, 1, ONENY)
         CALL QSVE (NAPRS1, 1, NAPRS1, WRK1)
C                                       Prepare NAPEXP for next pass.
         CALL QVADD (NAPEXP, 1, NAPEX1, 1, NAPEXP, 1, ONENY)
         CALL QWR
C                                       Get intermediate sum.
         IF (I.EQ.1) CALL QGET (WT1, NAPRS1, 1, 2)
 40      CONTINUE
C                                       Get sum of the weights.
      CALL QGET (WT, NAPRS1, 1, 2)
      CALL QWD
C                                       Compute weight over whole plane.
      WT = 2.0 * WT - WT1
      ARG = 1.0 / WT
      WT = LOG (ARG)
C                                       Store WT in AP loc 0.
      CALL QPUT (WT, 0, 1, 2)
      CALL QWD
C
 999  RETURN
      END
      SUBROUTINE SDICLN (NONEG, FINISH, IERR)
C-----------------------------------------------------------------------
C   SDICLN does a limited clean of the Steer/Dudney/Ito variety using
C   the highest map points.
C   INPUT:
C     NONEG  L     IF .TRUE. then stop at first negative component.
C     LUNCL1 I   = Logical unit number for reading the clean components.
C     LUNCL2 I   = Logical unit number for writing the clean components.
C     CLNFIL I   = File name array for the clean component file.
C     STGAIN R   = Clean loop gain for SDI
C     STFRAC R   = Clean clip level SDI
C     PTRK   I   = Number of map points in the AP.
C     PTRJ   I   = first location in the AP of the beam.
C     CLNSTR I   = First clean component to do. (0 for first clean).
C     CLNLIM I   = Maximum number of clean components desired.
C   OUTPUT:
C     CLNSTR I   = Current clean component number.
C     FINISH L   = .TRUE. If minimum clean component flux density
C                  or maximum iteration encountered, otherwise
C                  = .FALSE.
C     RESMAX R   = Maximum abs. residual (Jy).
C     A list of the clean components on file CLNFIL.
C-----------------------------------------------------------------------
      INCLUDE 'SDCLN.INC'
      CHARACTER KEYS(4)*8, PREFIX*5
      LOGICAL   NONEG, FINISH, LERR, FA, MAP, EXCL, WAIT, WASNEG
      INTEGER   ITER, ITERL, ITERM, IREC, LIREC, IGAIN, NPIXAV
      INTEGER   IBUFF(512), NREC, CATBLK(256), IRET, IERR, LREC, NKEY,
     *   KOLS(4), RAKOL, DECKOL, FLXKOL, TYPKOL, FIND, BIND, I, IBOX, J,
     *   JJ, JFLG, WIN(4), LOX, HIX, LOY, HIY, JBUFF(256), II, JJJ, JJ0,
     *   IFLGS(MABFSS), CONVCT(11), IYL, IYX, IPASS, MPASS, ALUN(2),
     *   AFIND(2), JCUR, ICUR, IROUND, ICR, LIN, LOU, BIND1, GLUN, IP,
     *   GFIND, NLPR, G2BUFF(256)
      REAL      RECORD(7), XFLUX, CATR(256), XCENTR, YCENTR, XBUFF(1),
     *   CBUFF(1), CONV(MABFSS,11), CONVMX, CONVMN, XXF, XXFMIN, AFACT,
     *   GBUFF(1), XXFAVG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK(1), CATR(1))
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL)
      EQUIVALENCE (XBUFF(1), BUFF1(1))
      EQUIVALENCE (IFLGS(1), BUFF2(1))
      EQUIVALENCE (CBUFF(1), BUFF3(1))
      EQUIVALENCE (G2BUFF(1), GBUFF(1))
C                                       All corrs applied at once
C                                       at IPASS=MPASS.  Therefore
C                                       MPASS must be 2.
      DATA MPASS /3/
      DATA FA /.FALSE. /
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ', 'TYPE OBJ'/
      DATA MAP, EXCL, WAIT /3*.TRUE./
C-----------------------------------------------------------------------
      CALL FILL (NBEMVL, 0, CONVCT)
      DO 5 I = 1,NBEMVL
         CALL RFILL (MABFSS, 0.0, CONV(1,I))
 5       CONTINUE
C                                      Open residual file.
      CALL ZOPEN (LUNRES, FIND, RESVOL, RESFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(2) = MINY
      WIN(3) = NX
      WIN(4) = MAXY
C                                      Initialize residual map.
      CALL MINIT ('READ', LUNRES, FIND, NX, NY, WIN, XBUFF, BUFSZ1,
     *   BORES, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
C                                       Open CLNFIL for reading
      NREC = 5000
      IF (CLNLIM-CLNSTR.LT.4990)  NREC = CLNLIM - CLNSTR + 10
      LREC = 3
      NKEY = 1
      CALL TABINI ('WRIT', 'CC', CLNVOL, FCNO(3), CLNVER, CATBLK,
     *   LUNCL2, NKEY, NREC, LREC, JBUFF, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      CLNSTR = CLNSTR + 1
      NKEY = 3
      TYPKOL = 7
      FLXKOL = 1
      RAKOL = 2
      DECKOL = 3
      IF (LREC.GT.3) NKEY = 4
      CALL FNDCOL (NKEY, KEYS, 8, FA, IBUFF, KOLS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1025) IRET
         CALL MSGWRT (7)
         END IF
      CALL RFILL (7, 0.0, RECORD)
      XCENTR = CATR(KRCRP)
      YCENTR = CATR(KRCRP+1)
C                                       Open 1st conv SC file
      IPASS = 0
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = MAXX - MINX + 1
      WIN(4) = MAXY - MINY + 1
      ALUN(1) = LUNRES + 1
      GLUN = ALUN(1) + 1
      CALL ZOPEN (ALUN(1), AFIND(1), CONVOL(1), CONFIL(1), MAP, EXCL,
     *   WAIT, IERR)
      IF (IERR.EQ.0) CALL ZOPEN (GLUN, GFIND, CONVOL(3), CONFIL(3),
     *   FA, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      CALL MINIT ('WRIT', ALUN(1), AFIND(1), WIN(3), WIN(4), WIN, CBUFF,
     *   BUFSZ3, BOCONV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR, IPASS
         GO TO 990
         END IF
C                                       Pass 0: read residual, write
C                                       CC (full int), convolve
      RESMAX = MAX (ABS(TVREMX), ABS(TVREMN))
      WASNEG = .FALSE.
      MAPLIM = RESMAX * STFRAC
      JFLG = 1
      ITER = CLNSTR - 1
      ITERL = CLNSTR
      CONVMX = -1.0E20
      CONVMN = 1.0E20
      ICR = -(NBEMVL/2)
C                                       Loop thru map looking for points
C                                       greater than MAPLIM.
      DO 100 I = MINY,MAXY
         IF (JFLG.GT.0) THEN
            JFLG = 0
            DO 45 J = MINX,MAXX
               IFLGS(J) = 0
 45            CONTINUE
            END IF
C                                      Read row of the map.
         IYL = MOD (I-MINY, NBEMVL)
         CALL MDISK ('READ', LUNRES, FIND, XBUFF, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR, I
            GO TO 990
            END IF
C                                       Loop over boxes: mask
         DO 65 IBOX = 1,NBOXS
            LOY = MIN (WINM(2,IBOX), WINM(4,IBOX))
            HIY = MAX (WINM(2,IBOX), WINM(4,IBOX))
            IF (HIY.LE.0) HIY = NY
            IF ((I.GE.LOY) .AND. (I.LE.HIY)) THEN
               LOX = MIN (WINM(1,IBOX), WINM(3,IBOX))
               HIX = MAX (WINM(1,IBOX), WINM(3,IBOX))
               DO 60 J = LOX,HIX
                  IFLGS(J) = 1
 60               CONTINUE
               JFLG = 1
               END IF
 65         CONTINUE
C                                       Get components
         IF (JFLG.LE.0) GO TO 100
         DO 80 J = MINX,MAXX
            IF (IFLGS(J).GT.0) THEN
               JJ = BIND + J - 1
               XXF = XBUFF(JJ)
               RESMAX = MAX (RESMAX, ABS(XXF))
               IF (ABS(XXF).GT.MAPLIM) THEN
                  ITER = ITER + 1
                  RECORD(TYPKOL) = 0
C                                       Stored RA and Dec refer to
C                                       the catalogd CLEAN map.
                  RECORD(RAKOL) = (J - XCENTR) * XSPACE
                  RECORD(DECKOL) = (I - YCENTR) * YSPACE
                  RECORD(FLXKOL) = XXF
                  IF (XXF.LT.0.0) WASNEG = .TRUE.
                  CALL TABIO ('WRIT', 0, ITER, RECORD, IBUFF, IERR)
                  IF (IERR.EQ.4) THEN
                     WRITE (MSGTXT,1065) ITER
                     CALL MSGWRT (8)
                     END IF
                  IF (IERR.NE.0) GO TO 999
C                                       convolve
                  IYX = IYL
                  JJ0 = J - (NBEMVL+1)/2
                  DO 75 II = 1,NBEMVL
                     IYX = IYX + 1
                     IF (IYX.GT.NBEMVL) IYX = 1
                     CONVCT(IYX) = 1
                     DO 70 JJ = 1,NBEMVL
                        JJJ = JJ0 + JJ
                        IF ((JJJ.GE.1) .AND. (JJJ.LE.MABFSS))
     *                     CONV(JJJ,IYX) = CONV(JJJ,IYX) +
     *                        XXF * BEMVAL(JJ,II)
 70                     CONTINUE
 75                  CONTINUE
                  END IF
               END IF
 80         CONTINUE
C                                       Write conv. row if needed, then
C                                       Drop/clear one rolling row
         IYX = IYL + 1
         ICR = ICR + 1
         IF (ICR.GT.0) THEN
            CALL MDISK ('WRIT', ALUN(1), AFIND(1), CBUFF, BIND1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR, IPASS
               GO TO 990
               END IF
            CALL RCOPY (WIN(3), CONV(MINX,IYX), CBUFF(BIND1))
            END IF
         IF (CONVCT(IYX).GT.0) THEN
            DO 95 J = MINX,MAXX
               CONVMX = MAX (CONVMX, CONV(J,IYX))
               CONVMN = MIN (CONVMN, CONV(J,IYX))
               CONV(J,IYX) = 0.0
 95            CONTINUE
            CONVCT(IYX) = 0.0
            END IF
 100     CONTINUE
C                                       Rest of convl. max
      DO 120 I = 1,NBEMVL
         IYX = IYX + 1
         IF (IYX.GT.NBEMVL) IYX = 1
         ICR = ICR + 1
         IF (ICR.LE.WIN(4)) THEN
            CALL MDISK ('WRIT', ALUN(1), AFIND(1), CBUFF, BIND1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR, IPASS
               GO TO 990
               END IF
            CALL RCOPY (WIN(3), CONV(MINX,IYX), CBUFF(BIND1))
            END IF
         IF (CONVCT(IYX).GT.0) THEN
            DO 115 J = MINX,MAXX
               CONVMX = MAX (CONVMX, CONV(J,IYX))
               CONVMN = MIN (CONVMN, CONV(J,IYX))
               CONV(J,IYX) = 0.0
 115           CONTINUE
            CONVCT(IYX) = 0
            END IF
 120     CONTINUE
C                                       Mark peak comp number
      ITERM = ITER
C                                       Close down pass 0
      CALL MDISK ('FINI', ALUN(1), AFIND(1), CBUFF, BIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR, IPASS
         GO TO 990
         END IF
C                                      Close residual file.
      CALL ZCLOSE (LUNRES, FIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1125) IERR
         GO TO 990
         END IF
C                                       Open 2nd SC file
      ALUN(2) = LUNRES
      CALL ZOPEN (ALUN(2), AFIND(2), CONVOL(2), CONFIL(2), MAP, EXCL,
     *   WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
C                                       Init IO pointers
      LIN = 1
      LOU = 2
      LIREC = 1
      NLPR = 256
C                                       Redo convolution
C                                       Rescale CC on last pass
      DO 220 IPASS = 1,MPASS
         XXFMIN = 1.5
         AFACT = 0.
         XXFAVG = 0.
         NPIXAV = 0
C                                       Init read
         CALL MINIT ('READ', ALUN(LIN), AFIND(LIN), WIN(3), WIN(4), WIN,
     *      XBUFF, BUFSZ1, BOCONV, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1135) IERR, IPASS
            GO TO 990
            END IF
C                                       Init write
         IF (IPASS.LT.MPASS) THEN
            CALL MINIT ('WRIT', ALUN(LOU), AFIND(LOU), WIN(3), WIN(4),
     *         WIN, CBUFF, BUFSZ3, BOCONV, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1035) IERR, IPASS
               GO TO 990
               END IF
            END IF
C                                       Read 1st CC
         ITER = ITERL
         CALL TABIO ('READ', 0, ITER, RECORD, IBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1145) IERR, ITER
            GO TO 990
            END IF
         JCUR = IROUND (RECORD(RAKOL)/XSPACE+XCENTR)
         ICUR = IROUND (RECORD(DECKOL)/YSPACE+YCENTR)
         ICR = -(NBEMVL/2)
C                                       Loop over rows
         DO 195 I = MINY,MAXY
            IYL = MOD(I-MINY, NBEMVL)
C                                       Read SC row
            CALL MDISK ('READ', ALUN(LIN), AFIND(LIN), XBUFF, BIND,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1150) IERR, IPASS
               GO TO 990
               END IF
C                                       Comp is in this row
 155        IF (I.EQ.ICUR) THEN
               IGAIN = ITER - ITERL + 1
               IREC = (IGAIN-1)/NLPR + 1
               IP = IGAIN - NLPR * (IREC-1)
               IF (IREC.NE.LIREC) THEN
                  CALL ZFIO ('WRIT', GLUN, GFIND, LIREC, G2BUFF, IERR)
                  LIREC = IREC
                  IF (IERR.EQ.0) CALL ZFIO ('READ', GLUN, GFIND, LIREC,
     *               G2BUFF, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1155) IERR
                     GO TO 990
                     END IF
                  END IF
               XXF = XBUFF(BIND+JCUR-MINX)
               IF (XXF.NE.0.0) XXF = RECORD(FLXKOL) / XXF
               IF (XXF.LE.0.0) XXF = 1.0
               IF (XXF.GT.1.5) XXF = 1.5
               XXFMIN = MIN (XXFMIN, XXF)
               AFACT = XXF ** 2 + AFACT
               XXFAVG = XXFAVG + XXF
               NPIXAV = NPIXAV + 1
               IF (IPASS.GT.1) XXF = XXF * GBUFF(IP)
               IF (XXF.GT.1.0) XXF = 1.0
               GBUFF(IP) = XXF
               XXF = RECORD(FLXKOL) * XXF
C                                       Write comp
               IF (IPASS.GE.MPASS) THEN
                  RECORD(FLXKOL) = XXF * STGAIN
                  FLUX = FLUX + RECORD(FLXKOL)
                  CALL TABIO ('WRIT', 0, ITER, RECORD, IBUFF, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1156) IERR, ITER
                     GO TO 990
                     END IF
                  END IF
C                                       Convolve to grid
               IF (IPASS.LT.MPASS) THEN
                  IYX = IYL
                  JJ0 = JCUR - (NBEMVL+1)/2
                  DO 165 II = 1,NBEMVL
                     IYX = IYX + 1
                     IF (IYX.GT.NBEMVL) IYX = 1
                     CONVCT(IYX) = 1
                     DO 160 JJ = 1,NBEMVL
                        JJJ = JJ0 + JJ
                        IF ((JJJ.GE.1) .AND. (JJJ.LE.MABFSS))
     *                     CONV(JJJ,IYX) = CONV(JJJ,IYX) +
     *                        XXF * BEMVAL(JJ,II)
 160                    CONTINUE
 165                 CONTINUE
                  END IF
C                                       Next record
               ITER = ITER + 1
               ICUR = 10000
               IF (ITER.LE.ITERM) THEN
                  CALL TABIO ('READ', 0, ITER, RECORD, IBUFF, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1145) IERR, ITER
                     GO TO 990
                     END IF
                  JCUR = IROUND (RECORD(RAKOL)/XSPACE+XCENTR)
                  ICUR = IROUND (RECORD(DECKOL)/YSPACE+YCENTR)
                  END IF
               GO TO 155
               END IF
C                                       Write conv. row if needed, then
C                                       Drop/clear one rolling row
            IF (IPASS.LT.MPASS) THEN
               IYX = IYL + 1
               ICR = ICR + 1
               IF (ICR.GT.0) THEN
                  CALL MDISK ('WRIT', ALUN(LOU), AFIND(LOU), CBUFF,
     *               BIND1, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1080) IERR, IPASS
                     GO TO 990
                     END IF
                  CALL RCOPY (WIN(3), CONV(MINX,IYX), CBUFF(BIND1))
                  END IF
               IF (CONVCT(IYX).GT.0) CALL RFILL (MABFSS, 0.0,
     *            CONV(1,IYX))
               CONVCT(IYX) = 0.0
               END IF
 195        CONTINUE
C                                       Rest of convl write
         IF (IPASS.LT.MPASS) THEN
            DO 210 I = 1,NBEMVL
               IYX = IYX + 1
               IF (IYX.GT.NBEMVL) IYX = 1
               ICR = ICR + 1
               IF (ICR.LE.WIN(4)) THEN
                  CALL MDISK ('WRIT', ALUN(LOU), AFIND(LOU), CBUFF,
     *               BIND1, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1080) IERR, IPASS
                     GO TO 990
                     END IF
                  CALL RCOPY (WIN(3), CONV(MINX,IYX), CBUFF(BIND1))
                  END IF
               IF (CONVCT(IYX).GT.0) CALL RFILL (MABFSS, 0.0,
     *            CONV(1,IYX))
               CONVCT(IYX) = 0
 210           CONTINUE
C                                       Close down pass IPASS
            CALL MDISK ('FINI', ALUN(LOU), AFIND(LOU), CBUFF, BIND1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR, IPASS
               GO TO 990
               END IF
            END IF
C                                       Switch in/out files
         I = LIN
         LIN = LOU
         LOU = I
         IF (NPIXAV.GT.0) XXFAVG = XXFAVG / NPIXAV
         IF (NPIXAV.GT.0) AFACT = AFACT / NPIXAV
         AFACT = AFACT - XXFAVG * XXFAVG
         IF (AFACT.GT.0.0) AFACT = SQRT (AFACT)
         IF (IPASS.EQ.1) THEN
            WRITE (MSGTXT,1215) IPASS, XXFMIN, XXFAVG, AFACT
         ELSE
            WRITE (MSGTXT,1216) IPASS, XXFMIN, XXFAVG, AFACT
            END IF
         CALL MSGWRT (4)
 220     CONTINUE
C                                       Are we done?
      CONVMX = MAX (ABS(CONVMX), ABS(CONVMN))
      IF (CONVMX.EQ.0.0) CONVMX = 1.0
      XXF = STGAIN * RESMAX / CONVMX
      IF (XXF.GT.STGAIN) XXF = STGAIN
      RESMAX = RESMAX - XXF * CONVMX
      FINISH = .TRUE.
      XFLUX = RESMAX
      CALL METSCA (XFLUX, PREFIX, LERR)
C                                       Check for iteration limit.
      CLNXST = CLNXST + (ITERM-ITERL+1) * XXF / GAIN
      IF (CLNXST+1.0.GE.CLNLIM) THEN
         WRITE (MSGTXT,1250) ITERM, XFLUX, PREFIX
C                                       Check for minimum CLEAN flux
      ELSE IF (RESMAX.LT.FMIN) THEN
         WRITE (MSGTXT,1255) XFLUX, PREFIX, ITERM
C                                       Check for negative comp.
      ELSE IF ((NONEG) .AND. (WASNEG)) THEN
         WRITE (MSGTXT,1260) XFLUX, PREFIX, ITERM
C                                       No just this major loop
      ELSE
         FINISH = .FALSE.
         WRITE (MSGTXT,1240) XFLUX, PREFIX, ITERM
         END IF
      CALL MSGWRT (4)
C                                       Write effective loop gain
      WRITE (MSGTXT,1265) XXF
      CALL MSGWRT (4)
C                                       Write total flux density
      XFLUX = FLUX
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1266) XFLUX, PREFIX
      CALL MSGWRT (4)
      ITER = CLNXST + 0.5
      WRITE (MSGTXT,1267) ITER
      CALL MSGWRT (4)
C                                       Output last buffer and close.
      CALL TABIO ('CLOS', 0, ITERM, RECORD, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1270) IERR
         GO TO 990
         END IF
C                                      Close scratch files.
      CALL ZCLOSE (ALUN(1), AFIND(1), IERR)
      IF (IERR.EQ.0) CALL ZCLOSE (ALUN(2), AFIND(2), IERR)
      IF (IERR.EQ.0) CALL ZCLOSE (GLUN, GFIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1280) IERR
         GO TO 990
         END IF
C                                       Set CLNSTR to the current clean
C                                       counter (NCOMP - 1 ).
      CLNSTR = ITERM
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDICLN: ERROR',I3,' OPENING RESIDUAL FILE')
 1015 FORMAT ('SDICLN: ERROR',I3,' INIT RESIDUAL FILE')
 1020 FORMAT ('SDICLN: ERROR',I3,' OPENING CC FILE ')
 1025 FORMAT ('SDICLN: ERROR',I3,' FINDING REQUIRED CC COLUMNS')
 1030 FORMAT ('SDICLN: ERROR',I3,' OPENING CONV SCRATCH FILE')
 1035 FORMAT ('SDICLN: ERROR',I3,' INIT WRITE CONV SCRATCH FILE, PASS',
     *   I3)
 1050 FORMAT ('SDICLN: RESIDUAL READ ERROR',I3,' ROW ',I5)
 1065 FORMAT ('SDICLN: ITER. ',I8,' CC FILE LIMIT EXCEEDED')
 1080 FORMAT ('SDICLN: ERROR',I3,' WRITE CONV SCRATCH FILE, PASS',I3)
 1125 FORMAT ('SDICLN: ERROR',I3,' CLOSING RESIDUAL FILE')
 1135 FORMAT ('SDICLN: ERROR',I3,' INIT READ CONV SCRATCH FILE, PASS',
     *   I3)
 1145 FORMAT ('SDICLN: ERROR',I7,' REREADING CC FILE ROW',I10,' ITER',
     *   I3)
 1150 FORMAT ('SDICLN: ERROR',I3,' READ CONV SCRATCH FILE, PASS',I3)
 1155 FORMAT ('SDICLN: ERROR',I3,' ON I/O TO CONV. CORR. FILE')
 1156 FORMAT ('SDICLN: ERROR',I7,' REWRITING CC FILE ROW',I10,' ITER',
     *   I3)
 1215 FORMAT ('Pass',I3,' min S.D. corr factor   ',F9.5,' avg',
     *   F8.4,' RMS',F8.4)
 1216 FORMAT ('Pass',I3,' min further corr factor',F9.5,' avg',
     *   F8.4,' RMS',F8.4)
 1240 FORMAT ('Reached approx. SDI flux =',F8.3,1X,A5,'Jy',
     *   ' iter =',I8)
 1250 FORMAT ('Reached iteration limit',I8,' max. res=',F8.3,1X,A5,
     *   'Jy')
 1255 FORMAT ('Reached min. CLEAN flux density=',F8.3,1X,A5,'Jy',
     *   ' iter=',I8)
 1260 FORMAT ('Included first negative, max.res=',F8.3,1X,A5,'Jy',
     *   ' iter=',I8)
 1265 FORMAT ('Effective loop gain this major cycle =',F8.5)
 1266 FORMAT ('Total CLEANed flux density =',F8.3,1X,A5,'Jy')
 1267 FORMAT ('at effective iteration =',I10)
 1270 FORMAT ('SDICLN: ERROR',I3,' CLOSING CC FILE')
 1280 FORMAT ('SDICLN: ERROR',I3,' CLOSING SCRATCH FILES')
      END
      SUBROUTINE SDITEL (FINISH, NORES, NONEG, ITV, IRET)
C-----------------------------------------------------------------------
C   SDITEL checks the TELL file to see if anything is waiting for SDCLN.
C   If so, it picks up the parms (via GTTELL), interprets them, and
C   sends them back to the rest of the task to handle.
C   In/out:   FINISH  L     Time to quit normally
C             NORES   L     Don't restore the components
C             NONEG   L     Quit on negative
C             ITV     I     =1 => do TV display
C   Output:   IRET    I     0 => okay, 99 => abort now
C-----------------------------------------------------------------------
      CHARACTER OPTELL*4
      LOGICAL   FINISH, NORES, NONEG
      INTEGER   ITV, IRET
      INTEGER   NPARMS, SCRTCH(256), IERR
      REAL      XGN, XFLUX, XNCONT, XSTFAC, XNITER, XBMAJ, XBMIN, XBPA,
     *   XFCTR, XMINPT, XMAXPX, XDOTV
      INCLUDE 'SDCLN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /SDITLL/ XGN, XFLUX, XNCONT, XSTFAC, XNITER, XBMAJ, XBMIN,
     *   XBPA, XFCTR, XMINPT, XMAXPX, XDOTV
C-----------------------------------------------------------------------
C                                       Check TELL file
      IRET = 0
      NPARMS = 12
      CALL GTTELL (NPARMS, OPTELL, XGN, SCRTCH, IERR)
      WRITE (MSGTXT,1000) OPTELL
      IF (IERR.NE.0) CALL MSGWRT (4)
      IF (IERR.EQ.2) FINISH = .TRUE.
      IF (IERR.EQ.3) IRET = 99
      IF (IERR.NE.1) GO TO 999
C                                       install parms
      FMIN = XFLUX
      IF (RESMAX.LE.FMIN) FINISH = .TRUE.
      STPNTS = XNCONT
      IF ((XSTFAC.GT.0.0) .AND. (XSTFAC.LT.STGAIN)) STGAIN = XSTFAC
C                                       number iterations, quit
      IF (XGN.GT.0.0) GAIN = XGN
      IF (XNITER.LE.-1.0) NONEG = .TRUE.
      IF (ABS(XNITER).GE.1.0) CLNLIM = ABS(XNITER) + 0.01
      IF (CLNXST.GT.CLNLIM) FINISH = .TRUE.
C                                       beam, no restore
      IF ((XBMAJ.NE.0.) .AND. (XBMIN.NE.0.0)) THEN
         IF (XBMAJ.LT.0.0) THEN
            NORES = .TRUE.
         ELSE
            GAUSA = XBMAJ
            GAUSB = XBMIN
            GAUSC = XBPA
            END IF
         END IF
      SPEXP  = XFCTR
      IF (XMINPT.GT.3) MINPCH = XMINPT - 0.9
      MAXPIX = XMAXPX
      ITV = 0
      IF (XDOTV.GT.0.0) ITV = 1
      IF (NTVDEV.LE.0) ITV = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RECEIVED ',A4,' OPERATION FROM TELL')
      END
