      PROGRAM UTESS
C-----------------------------------------------------------------------
C! Maximum emptiness deconvolution of an image.
C# Map AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2002-2003, 2006-2008, 2012, 2015
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
C   UTESS deconvolves an image by maximising the emptiness of an image.
C     This version performs tesselation of images, and allows asymmetric
C   beams. It also does convolutions the quick way. An output CLEAN-type
C   image is formed if BMAJ > 0.0.
C
C   INPUTS:
C   AIPS name    Prog. name     Description
C   INNAME       NAME(1)        Dirty image name (name)
C   INCLASS      CLASS(1)       Dirty image name (class)
C   INSEQ        SEQ(1)         Dirty image name (sequence number)
C   INDISK       VOL(1)         Dirty image name (disk number)
C   IN2NAME      NAME(2)        Beam image name (name)
C   IN2CLASS     CLASS(2)       Beam image name (class)
C   IN2SEQ       SEQ(2)         Beam image name (sequence number)
C   IN2DISK      VOL(2)         Beam image name (disk number)
C   IN3NAME      NAME(4)        Default image name (name)
C   IN3CLASS     CLASS(4)       Default image name (class)
C   IN3SEQ       SEQ(4)         Default image name (sequence number)
C   IN3DISK      VOL(4)         Default image name (disk number)
C   OUTNAME      NAME(3)        Output image name (name)
C   OUTCLASS     CLASS(3)       Output image name (class)
C   OUTSEQ       SEQ(3)         Output image name (sequence number)
C   OUTDISK      VOL(3)         Output image name (disk number)
C   OUT2NAME     NAME(5)        Restored image name (name)
C   OUT2CLAS     CLASS(5)       Restored image name (class)
C   OUT2SEQ      SEQ(5)         Restored image name (sequence number)
C   OUT2DISK     VOL(5)         Restored image name (disk number)
C   NMAPS        NFIELD         Number of fields to deconvolve
C   NITER        NITER          Maximum number of iterations
C   NOISE        FRES           Required rms noise (Jy/beam)
C   FLUX         FLUX           Zero spacing flux (Jy)
C   BLC          BLC            Bottom left-hand corner of image
C   TRC          TRC            Top right-hand corner of image
C   DOTV         ITV            TV display of image?
C   PRTLEV       LNGOUT         Extended print-out?
C   PBSIZE       BMSIZE         Primary beam size (arcsec) for each
C                                  input map
C   BMAJ         BMAJ           Major axis (arcsec) of convolving beam
C   BMIN         BMIN           Minor axis (arcsec) of convolving beam
C   BPA          BPA            PA (degrees) of convolving beam
C   BADDISK      IBAD           List of disks on which not to put
C                                  scratch files
C
C   Last Edit : 5-Apr-89
C
C   Programmer = T. J. Cornwell                         December 1987
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      INTEGER   TVPASS, I, IRET, LUN2, LUN1, IOCNT(2), USID, VOL(5),
     *   SEQ(5), CATBLK(256), KAP, TTIME(3), IFIELD, ITER, ITV, FIL
      LOGICAL   FINISH, OLD, CNVRGE
      REAL      CATR(256), DEFLEV, SCLF1, OSCLF, CPUTIM, FILMAX, FILMIN,
     *   TOL, FSMALL
      CHARACTER HILINE*72, CLASS(5)*6, NAME(5)*12
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DAPM.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATR(1), CATH(1), CATBLK(1))
      DATA FSMALL /-1.0E20/
C                                           Tolerance for solution
      DATA TOL/0.05/
C-----------------------------------------------------------------------
C
C *********************************************************************
C
C                        INITIALIZATION PHASE
C
C *********************************************************************
C
C                                       Initialize common constants
      T = .TRUE.
      F = .FALSE.
      MAP = .TRUE.
      EXCL = .TRUE.
      WAIT = .TRUE.
      IERR = 0
      IRET = 0
      LUN1 = 16
      LUN2 = 17
      ITER = 0
      NCFILE = 0
      FINISH = .FALSE.
      SCLF = 1.0
C                                       Get input values.
      CALL GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IRET)
C                                       Check for restart of AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFR1, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Get map files and create
C                                       output files if required.
C                                       Sets AP size too
      CALL FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (OLD) THEN
         IF (CHAN.EQ.1) THEN
            VMSTR = CATBLK(KINIT)
         ELSE
            VMSTR = 0
            ALPINI = 0.0
            BETINI = 0.0
            END IF
         IF (VMSTR.GT.0) ITER = VMSTR
         WRITE (MSGTXT,1000) VMSTR
         CALL MSGWRT (4)
         END IF
C                                       Write inputs to history and
C                                       log files.
      CALL VMHIS (NAME, VOL, CLASS, SEQ, OLD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initialize images
      IF (.NOT.DEFEXT) THEN
         DEFLEV = TFLUX / RWNXY
         CALL FLAT (DEFLEV, DEF, IERR)
         IF (IERR.NE.0) GO TO 990
         DEFMAX = DEFLEV
         DEFMIN = DEFLEV
      ELSE
C                                       Clip neg. from image.
         CALL COPMAP (DEF, DEF, FSMALL, DEFMAX, DEFMIN, IERR)
         IF (ITV.GT.0) THEN
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'UNABLE TO GET AP MEMORY FOR TV DISPLAY'
               CALL MSGWRT (8)
               IERR = 10
               GO TO 990
               END IF
            WRITE (MSGTXT,1010)
            CALL MSGWRT (4)
            TVPASS = -1
            CALL DISPTV (DEF, DEFMAX, DEFMIN, TVPASS)
            CALL QRLSE
            END IF
         END IF
C
      IF (.NOT.OLD) THEN
         CALL COPMAP (DEF, IMG, FSMALL, IMGMAX, IMGMIN, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL BGTOSM (VMOUT, IMG, IERR)
         CALL COPMAP (IMG, IMG, FSMALL, IMGMAX, IMGMIN, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (IMGMAX.LE.FSMALL) THEN
            CALL COPMAP (DEF, IMG, FSMALL, IMGMAX, IMGMIN, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      IMGMIN = MAX(IMGMIN, FSMALL)
C                                       Calculate residuals of image
      CALL CALRES(IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set initial values of
C                                       Lagrange multipliers
      ALPHA = 0.0
      IF (ALPINI.GT.0.0) ALPHA = ALPINI
      TVPASS = 1
C                                       Stop if default image fits OK
      IF (FITRMS.LT.TOL) THEN
         WRITE (MSGTXT,1200) FITRMS
         CALL MSGWRT (4)
         IF (DOSTOP) THEN
            CNVRGE=.TRUE.
            DOSTOP=.TRUE.
            FINISH = .TRUE.
            GO TO 200
            END IF
         END IF
C                                       Find first changes to ALPHA
      CALL CHALBT (RES, IMG, DEF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        Output useful info
      WRITE (MSGTXT,1001)
      CALL MSGWRT (4)
      IF (LNGOUT) THEN
         WRITE (MSGTXT,1035) GRDJSQ
         CALL MSGWRT (4)
         WRITE (MSGTXT,1037) GRDHSQ,GRDESQ
         CALL MSGWRT (4)
         WRITE (MSGTXT,1040) RNPPB,SCLF
         CALL MSGWRT (4)
         WRITE (MSGTXT,1050) RL0
         CALL MSGWRT (4)
         WRITE (MSGTXT,1060) ALPHA
         CALL MSGWRT (4)
         DO 104 IFIELD=1,NFIELD
            WRITE (MSGTXT,2006) IFIELD, FFIT(IFIELD)
            CALL MSGWRT (4)
 104     CONTINUE
      ELSE
         WRITE (MSGTXT,1070) EPSSTR,RL0,RNPPB
         CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1080) FLUX,FITRMS
      CALL MSGWRT (4)
      WRITE (MSGTXT,1090) IMGMAX,IMGMIN
      CALL MSGWRT (4)
C *********************************************************************
C
C                        ITERATION PHASE
C
C *********************************************************************
 100  CONTINUE
C                                       Check TELL file
         CALL VTTELL (ITER, FINISH, ITV, TVPASS, IRET)
         IF (IRET.NE.0) GO TO 990
         ITER = ITER + 1
         IF (ITER.GE.VMLIM) FINISH = T
         CALL ZCPU (CPUTIM, IOCNT)
         CALL ZTIME (TTIME)
         WRITE (MSGTXT,1020) ITER, TTIME(1), TTIME(2), TTIME(3),
     *     CPUTIM
         CALL MSGWRT (4)
C                                       Calculate step
         CALL CALSTP (RES, IMG, DEF, DEL, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Take step
         CALL STEP (IMG, DEL, 0.1*IMGMIN+FSMALL, SCLF, IERR)
         IF (IERR.NE.0) GO TO 990
         OSCLF = SCLF
C                                       Recalculate residuals
      CALL CALRES(IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check TELL file
      CALL VTTELL (ITER, FINISH, ITV, TVPASS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Find gradJ dot step to
C                                        determine optimum step length
      CALL DOTGRD (RES, IMG, DEF, DEL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                         Rescale
      RNPPB = RNPPB * (1.0/MAX(0.5,MIN(2.0,EPSSTR)) + 1.0) / 2
C                                        Step back to new place
      IF ((EPSSTR.NE.0.0).AND.(ABS(EPSSTR-1.0).GT.GAIN)) THEN
         SCLF = 1.0
         IF (RL0.GT.0.0) SCLF = EPSSTR*OSCLF
         SCLF1 = SCLF - OSCLF
         CALL STEP (IMG, DEL, 0.1*IMGMIN+FSMALL, SCLF1, IERR)
         IF (IERR.NE.0) GO TO 990
C                                        Find residuals of new image
         CALL CALRES(IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                        Find Current gradient
      CALL CHALBT (RES, IMG, DEF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                         Output useful info
      IF (LNGOUT) THEN
         WRITE (MSGTXT,1030) EPSSTR, RJ0, RJ1
         CALL MSGWRT (4)
         WRITE (MSGTXT,1035) GRDJSQ
         CALL MSGWRT (4)
         WRITE (MSGTXT,1037) GRDHSQ, GRDESQ
         CALL MSGWRT (4)
         WRITE (MSGTXT,1040) RNPPB, SCLF
         CALL MSGWRT (4)
         WRITE (MSGTXT,1050) RL0
         CALL MSGWRT (4)
         WRITE (MSGTXT,1060) ALPHA
         CALL MSGWRT (4)
         DO 204 IFIELD=1,NFIELD
            WRITE (MSGTXT,2006) IFIELD, FFIT(IFIELD)
            CALL MSGWRT (4)
 204     CONTINUE
      ELSE
         WRITE (MSGTXT,1070) EPSSTR, RL0, RNPPB
         CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1080) FLUX, FITRMS
      CALL MSGWRT (4)
      WRITE (MSGTXT,1090) IMGMAX, IMGMIN
      CALL MSGWRT (4)
C                                        If appropiate suggest stopping
         CNVRGE = (FITRMS.LT.TOL) .AND.
     *      (ITER.NE.1) .AND. (ITER.NE.VMSTR) .AND. (RL0.LT.TOL)
         IF (CNVRGE) THEN
            IF (DOSTOP) THEN
               FINISH = T
            ELSE
               GAIN = RL0
               END IF
            END IF
C                                       Display current UTESS map.
         IF (ITV.GT.0) THEN
            FIL = IMG
            FILMAX = IMGMAX
            FILMIN = IMGMIN
            IF (ITV.EQ.2) FIL = RES
            IF (ITV.EQ.2) FILMAX = RESMAX
            IF (ITV.EQ.2) FILMIN = RESMIN
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'UNABLE TO GET AP MEMORY FOR TV DISPLAY'
               CALL MSGWRT (8)
               IERR = 10
               GO TO 990
               END IF
            IF (FINISH) TVPASS = -2
            CALL DISPTV (FIL, FILMAX, FILMIN, TVPASS)
            IF (TVPASS.EQ.32700)  FINISH = T
            TVPASS = 2
            CALL QRLSE
            END IF
C                                       Output Current UTESS map
 200     CONTINUE
         CALL CATIO ('READ', VOL(3), SCRCNO(SCRNM(VMOUT)), CATBLK,
     *     'REST', BUFFR3, IERR)
         IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
            WRITE (MSGTXT,1120) IERR
            CALL MSGWRT (4)
            IERR = 0
            END IF
         CATBLK(KINIT) = ITER
C  May Problem here:
         I = 255
         CATR(I) = ALPHA
         CATR(KRDMX) = IMGMAX
         CATR(KRDMN) = MIN (IMGMIN, 0.0)
         CATBLK(KITYP) = 4
C                                       Update UTESS catalog header
         CALL CATIO ('UPDT', VOL(3), SCRCNO(SCRNM(VMOUT)), CATBLK,
     *      'REST', BUFFR3, IERR)
         IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
            WRITE (MSGTXT,1120) IERR
            CALL MSGWRT (4)
            IERR = 0
            END IF
C                                       Write into output file.
      CALL SMTOBG (IMG, 0, VMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       If more VM required, loop
      IF (.NOT.FINISH) GO TO 100
C
C
C *********************************************************************
C
C                        FINISHING UP
C
C *********************************************************************
C
C                                       Diagnose result
      IF (FITRMS.LT.TOL) THEN
         WRITE (MSGTXT,1340)
         CALL MSGWRT (4)
         IF (RL0.LT.TOL) THEN
            WRITE (MSGTXT,1320) RL0, TOL
            CALL MSGWRT (4)
            WRITE (MSGTXT,1300)
            CALL MSGWRT (4)
         ELSE
            WRITE (MSGTXT,1330) RL0, TOL
            CALL MSGWRT (4)
            WRITE (MSGTXT,1310)
            CALL MSGWRT (4)
            END IF
      ELSE
         WRITE (MSGTXT,1350)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1310)
         CALL MSGWRT (4)
         END IF
C
      IF (.NOT.LNGOUT) THEN
         DO 304 IFIELD=1,NFIELD
            WRITE (MSGTXT,2006) IFIELD, FFIT(IFIELD)
            CALL MSGWRT (4)
 304     CONTINUE
         END IF
C
C                                       Put stuff in history file
      CALL HIOPEN (LUN1, VOL(3), SCRCNO(SCRNM(VMOUT)), BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 260
      WRITE (HILINE,1130) TSKNAM, FITRMS
      CALL HIADD (LUN1, HILINE, BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 260
      WRITE (HILINE,1140) TSKNAM, FLUX
      CALL HIADD (LUN1, HILINE, BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 260
      WRITE (HILINE,1150) TSKNAM, RL0
      CALL HIADD (LUN1, HILINE, BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 260
      WRITE (HILINE,1160) TSKNAM, ITER
      CALL HIADD (LUN1, HILINE, BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 260
      WRITE (HILINE,1170) TSKNAM, ALPHA
      CALL HIADD (LUN1, HILINE, BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 260
      WRITE (HILINE,1190) TSKNAM, RNPPB
      CALL HIADD (LUN1, HILINE, BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 260
C
 260  CALL HICLOS (LUN1, T, BUFFR1, IERR)
C
      IF (BMAJ.GT.0.0) THEN
         CALL MAKCVM (IMG, DEL, IERR)
         CALL CATIO ('READ', VOL(5), SCRCNO(SCRNM(CVMOUT)), CATBLK,
     *      'REST', BUFFR3, IERR)
         CALL SMTOBG (DEL, 0, CVMOUT, IERR)
         CALL KEYPCP (VOL(3), SCRCNO(SCRNM(VMOUT)), VOL(5),
     *      SCRCNO(SCRNM(CVMOUT)), 0, ' ', IERR)
         CALL HISCOP (LUN2, LUN1, VOL(3), VOL(5), SCRCNO(SCRNM(VMOUT)),
     *       SCRCNO(SCRNM(CVMOUT)), CATBLK, BUFFR1, BUFFR2, IERR)
C                                       HISCOP doesn't close copied
C                                       history file; add HICLOS.
         CALL HICLOS (LUN1, T, BUFFR1, IERR)
C                                       Update header
         IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
            WRITE (MSGTXT,1120) IERR
            CALL MSGWRT (4)
            IERR = 0
            END IF
         CATBLK(KINIT) = ITER
         CATR(KRDMX) = RESMAX
         CATR(KRDMN) = RESMIN
         CATR(KRBMJ) = BMAJ/3600.0
         CATR(KRBMN) = BMIN/3600.0
         CATR(KRBPA) = BPA
         CATBLK(KITYP) = 4
C                                       Update catalog header
         CALL CATIO ('UPDT', VOL(5), SCRCNO(SCRNM(CVMOUT)), CATBLK,
     *      'REST', BUFFR3, IERR)
         IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
            WRITE (MSGTXT,1120) IERR
            CALL MSGWRT (4)
            IERR = 0
            END IF
         END IF
C                                        Finished.
 990  IRET = IERR
      CALL DIE (IRET, BUFFR1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Restarting UTESS after ',I5,' iterations')
 1001 FORMAT ('Initial conditions :')
 1010 FORMAT ('Displaying default image')
 1020 FORMAT ('Iteration :',I3,'  at ',I2,':',I2,':',I2,
     *   '    CPU time : ',F10.2)
 1030 FORMAT ('  Step length,J0,J1:',3(1X,1PE11.3))
 1035 FORMAT ('  Grad. J     : ',1PE11.3)
 1037 FORMAT ('  Grad. H,E    : ',2(1X,1PE11.3))
 1040 FORMAT ('  Npoints : ',1PE11.3,' SCLF : ',1PE11.3)
 1050 FORMAT ('  GradienT : ',1PE11.3)
 1060 FORMAT ('  Alpha : ',1PE12.3)
 1070 FORMAT ('  Step, gradient, npoints : ',F8.3,1X,F9.5,1X,F8.2)
 1080 FORMAT ('  Flux, error achieved : ',1PE11.3,' Jy,',
     *         1PE11.3,' sigma')
 1090 FORMAT ('  Image max,min   : ',2(1PE11.3,1X),' Jy/pixel')
 2006 FORMAT ('  Field',I5,' : error = ',1PE11.3,' sigma')
 1120 FORMAT ('ERROR',I3,' UPDATING UTESS HEADER ')
 1130 FORMAT (A6,'/ RMS residual =',1PE12.4,' sigma')
 1140 FORMAT (A6,'/ Total flux = ',1PE12.4,' Jy')
 1150 FORMAT (A6,'/ Gradient =',1PE11.3)
 1160 FORMAT (A6,'/ Number of iterations = ',I4)
 1170 FORMAT (A6,'/ Final alpha = ',1PE12.4)
 1190 FORMAT (A6,'/ Final npoints = ',1PE12.4)
 1200 FORMAT ('  Initial image fits data : error = ',1PE12.4,
     *   ' sigma')
 1300 FORMAT ('*** Good solution obtained ***')
 1310 FORMAT ('*** NO SOLUTION OBTAINED: REVISE INPUTS AND',
     *   ' TRY AGAIN ***')
 1320 FORMAT ('*** Final gradient = ',F7.4,' < ',F7.4,' ***')
 1330 FORMAT ('*** Final gradient = ',F7.4,' > ',F7.4,' ***')
 1340 FORMAT ('*** Reached target flux and fit ***')
 1350 FORMAT ('*** FAILED TO REACH TARGET FLUX AND FIT ***')
      END
      SUBROUTINE CALSTP (IN1, IN2, IN3, OUT, IERR)
C-----------------------------------------------------------------------
C   CALSTP finds the gradient search direction by using the Newton-
C   Raphson method. The part of the Hessian due to the beam sidelobes
C   is neglected.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN1, IN2, IN3, OUT, IFIELD
      INTEGER   AKOPEN, AKCESS, AKCLOS
      REAL      RHESSN, GRADH, GRADER, LENGTH, RTEMP, RTMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      GRDHSQ = 0.0
      GRDESQ = 0.0
      GRDHE =  0.0
      FLUX = 0.0
      RTMP = 0.0
      DO 10 IFIELD=1,NFIELD
         RTMP = RTMP + RVAR(IFIELD)/FRES(IFIELD)**2
 10      CONTINUE
      RTEMP = 2*ALPHA*RNPPB*RTMP
      RTMP = SQRT (RTMP)*RNPPB / MAX(1.0, FITRMS)
      IF (AKOPEN (IN1,1,'READ',BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (IN2,2,'READ',BUFFR2).NE.0) GO TO 990
      IF (AKOPEN (IN3,3,'READ',BUFFR3).NE.0) GO TO 990
      IF (AKOPEN (OUT,4,'WRIT',BUFFR4).NE.0) GO TO 990
C                                       Loop through map
      DO 200 IY = 1, HNY
         IF (AKCESS (OUT, BUFFR4) .NE.0) GO TO 990
         CALL RFILL (HNY, 0.0, BUFFR4(BIND(4)))
         IF ((IY.LT.YBEG).OR.(IY.GT.YEND)) GO TO 200
         IF (AKCESS (IN1, BUFFR1) .NE.0) GO TO 990
         IF (AKCESS (IN2, BUFFR2) .NE.0) GO TO 990
         IF (AKCESS (IN3, BUFFR3) .NE.0) GO TO 990
C                                        Do calculations
         I1 = BIND(1)
         I2 = BIND(2)
         I3 = BIND(3)
         I4 = BIND(4) + XBEG - 1
         DO 190 IX = XBEG,XEND
C
            FLUX = FLUX + BUFFR2(I2)
            GRADH = -TANH((BUFFR2(I2) - BUFFR3(I3))*RTMP)
            RHESSN = 1.0 / ((1-GRADH**2) * RTMP + RTEMP )
            GRADER = BUFFR1(I1)
            GRDHSQ = GRDHSQ + RHESSN * GRADH**2
            GRDESQ = GRDESQ + RHESSN * GRADER**2
            GRDHE = GRDHE + RHESSN * GRADER * GRADH
            BUFFR4(I4) = RHESSN * (GRADH - ALPHA*BUFFR1(I1))
            I1 = I1 + 1
            I2 = I2 + 1
            I3 = I3 + 1
            I4 = I4 + 1
 190        CONTINUE
 200     CONTINUE
C
      IF (AKCLOS (IN1, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (IN2, BUFFR2) .NE.0) GO TO 990
      IF (AKCLOS (IN3, BUFFR3) .NE.0) GO TO 990
      IF (AKCLOS (OUT, BUFFR4) .NE.0) GO TO 990
C
      IF (GRDHSQ.LT.1E-10) GRDHSQ = 0.0
      LENGTH = GRDHSQ + ALPHA**2*GRDESQ
      IF (LENGTH.LE.0.0) THEN
         LENGTH = GRDESQ
         END IF
      GRDJSQ = GRDHSQ + ALPHA**2*GRDESQ - 2.0*ALPHA*GRDHE
C
      RL0 = 0.0
      IF (GRDJSQ.LE.1E-8) GRDJSQ = 0.0
      IF (GRDJSQ.GT.0.0) RL0 = GRDJSQ/LENGTH
      IF (ALPHA.EQ.0.0) RL0 = 0.0
      RJ0 = GRDJSQ
      SCLF = 1.0
      IF (RL0.GT.0.0) SCLF = MIN (1.0, GAIN/RL0)
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CALSTP')
      END
      SUBROUTINE CHALBT (IN1, IN2, IN3, IERR)
C-----------------------------------------------------------------------
C   CHALBT calculate the norms and cross norms of the gradient vectors
C   in order to calculate how much to change ALPHA
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN1, IN2, IN3, IFIELD
      INTEGER   AKOPEN, AKCESS, AKCLOS
      REAL      RHESSN, GRADH, GRADER, B2M4AC, LENGTH, RTEMP, DELE, RTMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      GRDHSQ = 0.0
      GRDESQ = 0.0
      GRDHE =  0.0
      FLUX = 0.0
      RTMP = 0.0
      DO 10 IFIELD = 1,NFIELD
         RTMP = RTMP + RVAR(IFIELD) / FRES(IFIELD)**2
 10      CONTINUE
      RTEMP = 2*ALPHA*RNPPB*RTMP
      RTMP = SQRT (RTMP)*RNPPB / MAX (1.0, FITRMS)
      IF (AKOPEN (IN1,1,'READ',BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (IN2,2,'READ',BUFFR2).NE.0) GO TO 990
      IF (AKOPEN (IN3,3,'READ',BUFFR3).NE.0) GO TO 990
C                                       Loop through map
      DO 200 IY = YBEG,YEND
         IF (AKCESS (IN1, BUFFR1) .NE.0) GO TO 990
         IF (AKCESS (IN2, BUFFR2) .NE.0) GO TO 990
         IF (AKCESS (IN3, BUFFR3) .NE.0) GO TO 990
C                                        Do calculations
         I1 = BIND(1)
         I2 = BIND(2)
         I3 = BIND(3)
         DO 190 IX = XBEG,XEND
C
            FLUX = FLUX + BUFFR2(I2)
            GRADH = -TANH ((BUFFR2(I2) - BUFFR3(I3))*RTMP)
            RHESSN = 1.0 / ((1-GRADH**2) * RTMP + RTEMP )
            GRADER = BUFFR1(I1)
            GRDHSQ = GRDHSQ + RHESSN * GRADH**2
            GRDESQ = GRDESQ + RHESSN * GRADER**2
            GRDHE = GRDHE + RHESSN * GRADER * GRADH
            I1 = I1 + 1
            I2 = I2 + 1
            I3 = I3 + 1
 190        CONTINUE
 200     CONTINUE
C
      IF (AKCLOS (IN1, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (IN2, BUFFR2) .NE.0) GO TO 990
      IF (AKCLOS (IN3, BUFFR3) .NE.0) GO TO 990
C
      IF (GRDHSQ.LT.1E-10) GRDHSQ = 0.0
      GRDJE = GRDHE - ALPHA*GRDESQ
      LENGTH = GRDHSQ + ALPHA**2*GRDESQ
      IF (LENGTH.LE.0.0) THEN
         LENGTH = GRDESQ
         END IF
      GRDJSQ = GRDHSQ + ALPHA**2*GRDESQ - 2.0*ALPHA*GRDHE
      RL0 = 0.0
      IF ((LENGTH.GT.0.0) .AND. (GRDJSQ.GT.0.0)) RL0 = GRDJSQ/LENGTH
      IF (ALPHA.EQ.0.0) RL0 = 0.0
      IF (RL0.GT.GAIN) GO TO 300
         B2M4AC = GRDJE**2 - (GRDJSQ-GAIN*LENGTH)*GRDESQ
         IF (B2M4AC.GT.0.0) THEN
            DELAMX = (GRDJE+SQRT (B2M4AC)) / GRDESQ
            DELAMN = (GRDJE-SQRT (B2M4AC)) / GRDESQ
         ELSE
            DELAMX = 0.0
            DELAMN = 0.0
            END IF
         DELE = NFIELD*RWNXY*FITRMS**2/RNPPB + GRDJE
         DELALP = DELE/GRDESQ
         DELALP = MAX(DELAMN, MIN(DELAMX, DELALP))
         ALPHA = MAX (0.0, ALPHA + DELALP)
      GO TO 999
C
 300  CONTINUE
C                                      Doing poorly : choose
C                                      ALPHA to minimise
C                                      the gradient
      ALPHA = GRDHE / GRDESQ
      ALPHA = MAX (ALPHA, 0.0)
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHALBT')
      END
      SUBROUTINE DOTGRD (IN1, IN2, IN3, IN4, IERR)
C-----------------------------------------------------------------------
C   DOTGRD calculate the norms and cross norms of the gradient vectors
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN1, IN2, IN3, IN4, IFIELD
      INTEGER   AKOPEN, AKCESS, AKCLOS
      REAL      GRADH, RTEMP, RTMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      RJ1 = 0.0
      FLUX = 0.0
      RTMP = 0.0
      DO 10 IFIELD = 1,NFIELD
         RTMP = RTMP + RVAR(IFIELD) / FRES(IFIELD)**2
 10      CONTINUE
      RTEMP = 2*ALPHA*RNPPB*RTMP
      RTMP = SQRT (RTMP)*RNPPB / MAX (1.0, FITRMS)
      IF (AKOPEN (IN1,1,'READ',BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (IN2,2,'READ',BUFFR2).NE.0) GO TO 990
      IF (AKOPEN (IN3,3,'READ',BUFFR3).NE.0) GO TO 990
      IF (AKOPEN (IN4,4,'READ',BUFFR4).NE.0) GO TO 990
C                                       Loop through map
      DO 200 IY = YBEG,YEND
         IF (AKCESS (IN1, BUFFR1) .NE.0) GO TO 990
         IF (AKCESS (IN2, BUFFR2) .NE.0) GO TO 990
         IF (AKCESS (IN3, BUFFR3) .NE.0) GO TO 990
         IF (AKCESS (IN4, BUFFR4) .NE.0) GO TO 990
C                                        Do calculations
         I1 = BIND(1)
         I2 = BIND(2)
         I3 = BIND(3)
         I4 = BIND(4)
         DO 190 IX = XBEG,XEND
C
            GRADH = - TANH((BUFFR2(I2) - BUFFR3(I3))*RTMP)
            RJ1 = RJ1 + BUFFR4(I4)*(GRADH - ALPHA*BUFFR1(I1))
            I1 = I1 + 1
            I2 = I2 + 1
            I3 = I3 + 1
            I4 = I4 + 1
 190        CONTINUE
 200     CONTINUE
C
      IF (AKCLOS (IN1, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (IN2, BUFFR2) .NE.0) GO TO 990
      IF (AKCLOS (IN3, BUFFR3) .NE.0) GO TO 990
      IF (AKCLOS (IN4, BUFFR4) .NE.0) GO TO 990
C
      EPSSTR = 0.0
      IF (RJ1.NE.RJ0) EPSSTR = RJ0 / (RJ0-RJ1)
      IF (RL0.GT.0.0) THEN
         IF (SCLF.NE.0.0) THEN
            EPSSTR = MIN (EPSSTR, GAIN/(SCLF*RL0))
         ELSE
            EPSSTR = MIN (1.0, EPSSTR)
            END IF
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DOTGRD')
      END
      SUBROUTINE FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   FILES reads the beam and the dirty map, opens the
C   default map, creates/opens the UTESS map, and creates the scratch
C   files. The dirty map, default map, and VM map are copied into small
C   scratch files.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      CHARACTER NAME(5)*12, CLASS(5)*6, STAT*4, MAPUNT*8, CMPUNT*8,
     *   MTYPE*2
      INTEGER   USID, CATBLK(256), CNO, VOL(5), SEQ(5), OLDIM,
     *   OLDNAX(7), IDEPTH(5), CATDEF(256), CATIMG(256), IDEL, JDEL, I,
     *   IROUND, LUN1, LUN2, FIL, CORN(7), ISIZE, WIN(4), XOFF, YOFF,
     *   IFIELD, KAP, IDIR, JERR, ITMP, LF, NEED, MNAX(2), NOMAT
      LOGICAL   OLD
      DOUBLE PRECISION CATD(128), OLDCRV(7), FREQ, MCRV(2)
      HOLLERITH CATH(256)
      REAL      CATR(256), XBUF1(1), RMAX, RMIN, XCEN, YCEN, INDEF,
     *   MCIC(2), MCRP(2), MEPS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (BUFFR1, XBUF1)
      DATA IDEPTH /5*1/
      DATA MAPUNT /'JY/PIXEL'/
      DATA CORN /7*1/
      DATA CMPUNT /'JY/BEAM '/
C-----------------------------------------------------------------------
      INDEF = FBLANK
C
      IERR = 0
      NCFILE = 0
      LUN1 = 16
      LUN2 = 17
      NOMAT = 0
C                                       Read beam.
C                                       Get catalog slot for first BEAM.
      CNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', VOL(2), CNO, NAME(2), CLASS(2), SEQ(2),
     *    MTYPE, USID, STAT, BUFFR3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(2), CLASS(2), SEQ(2), VOL(2),
     *      USID
         GO TO 990
         END IF
C                                       Copy CATBLK and mark beam READ
      CALL CATIO ('READ', VOL(2), CNO, CATBLK, 'REST', BUFFR1(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Determine NX and NY
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Get cell size in arcseconds
      CELLX = CATR(KRCIC) * 3600.0
      CELLY = CATR(KRCIC+1) * 3600.0
C                                       set array name
      CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Set up windows
      IF ((BLC(1).LE.0) .OR. (BLC(1).GE.NX)) BLC(1) = 1 + NX/4
      IDEL = NX/2-1
      IF (TRC(1).GT.BLC(1)) IDEL = TRC(1)-BLC(1)
      TRC(1) = BLC(1) + IDEL
      IF ((BLC(2).LE.0) .OR. (BLC(2).GE.NY)) BLC(2) = 1 + NY/4
      JDEL = NY/2-1
      IF (TRC(2).GT.BLC(2)) JDEL = TRC(2) - BLC(2)
      TRC(2) = BLC(2) + JDEL
      XBEG = 1 + NX/4 - (TRC(1)-BLC(1)+1)/2
      YBEG = 1 + NY/4 - (TRC(2)-BLC(2)+1)/2
      XEND = XBEG + (TRC(1)-BLC(1))
      YEND = YBEG + (TRC(2)-BLC(2))
C
      RWNXY = REAL(TRC(1)-BLC(1)+1) * REAL(TRC(2)-BLC(2)+1)
C
      DO 10 I = 1,NUMFIL
         VMSZ(1,I) = NX / 2
         VMSZ(2,I) = NY / 2
 10      CONTINUE
C
      DO 20 IFIELD=1,NFIELD
         VMSZ(1,DAT(IFIELD)) = NX
         VMSZ(2,DAT(IFIELD)) = NY
         VMSZ(1,WT(IFIELD)) = NY * 2
         VMSZ(2,WT(IFIELD)) = NX / 2 + 1
 20      CONTINUE
C
      VMSZ(1,WK1) = NY * 2
      VMSZ(2,WK1) = NX / 2 + 1
      VMSZ(1,WK2) = NY * 2
      VMSZ(2,WK2) = NX / 2 + 1
      VMSZ(1,WK3) = NY * 2
      VMSZ(2,WK3) = NX / 2 + 1
      VMSZ(1,VMOUT) = NX
      VMSZ(2,VMOUT) = NY
      VMSZ(1,CVMOUT) = NX
      VMSZ(2,CVMOUT) = NY
      HNX = NX/2
      HNY = NY/2
C                                       Create scratch files.
      NSCR = 0
      DO 30 FIL = 1,NUMSCR
         CALL MAPSIZ (2, VMSZ(1,FIL), ISIZE)
         CALL SCREAT (ISIZE, BUFFR1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, FIL
            GO TO 990
            END IF
         SCRNM(FIL) = NSCR
         VMVOL(FIL) = SCRVOL(NSCR)
         CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, VMFILE(FIL),
     *      IERR)
 30      CONTINUE
C
      SCRNM(VMOUT)  = NSCR + 1
      SCRNM(CVMOUT) = NSCR + 2
C                                       Loop through input beams
      DO 50 IFIELD = 1,NFIELD
         CNO = 1
         ITMP = SEQ(2) + IFIELD - 1
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', VOL(2), CNO, NAME(2), CLASS(2), ITMP,
     *      MTYPE, USID, STAT, BUFFR3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, NAME(2), CLASS(2), ITMP, VOL(2),
     *         USID
            GO TO 990
            END IF
C                                       Copy CATBLK
         CALL CATIO ('READ', VOL(2), CNO, CATBLK, 'READ', BUFFR3,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Mark file 'READ' in /CFILES/
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL(2)
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 0
C                                       Use WK1 file for temp BEAM.
         VMSZ(1,WK1) = NX
         VMSZ(2,WK1) = NY
         VMSZ(1,WK2) = NX
         VMSZ(2,WK2) = NY
C                                       If interferometric data then
C                                       shift beam. I don't like this
C                                       but crazy handling of beams
C                                       in AIPS should be changed
C                                       sometime.
C                                       The pipeline made this
C                                       necessary!
C         IF (BMSIZE(IFIELD).GE.0) THEN
         CALL PEAKFN (LUN1, VOL(2), CNO, IDEPTH, CATBLK, BUFFR3,
     *      BUFSZ(3), XCEN, YCEN, IERR)
         IF (IERR.NE.0) GO TO 990
         XCEN = XCEN - NX/2 - 1
         YCEN = YCEN - NY/2 - 1
         ICENX = IROUND (XCEN)
         ICENY = IROUND (YCEN)
         XOFF = ICENX
         YOFF = ICENY
C         ELSE
C            XOFF = 0
C            YOFF = 0
C            END IF
C                                       Read into scratch file for FFT
         CHAN = MIN(CHAN, CATBLK(KINAX+2))
         CORN(3) = CHAN
C
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NX
         WIN(4) = NY
C
         CALL PLNGET (VOL(2), CNO, CORN, WIN, XOFF, YOFF, SCRNM(WK1),
     *      VMSZ(1,WK1), VMSZ(2,WK1), BUFFR1, BUFFR2, BUFSZ(1),
     *      BUFSZ(2), LUN1, LUN2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1035) IERR
            GO TO 990
            END IF
C                                       If required fit beam
C
         IF ((IFIELD.EQ.1).AND.(BMAJ.EQ.0.0)) THEN
            CALL BMSHP(WK1, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       Calculate transfer function. Use
C                                       full transform to allow
C                                       asymmetric PSFs
         NEED = 4 * NY * (NX/2 + 1)
         NEED = NEED / 1024
         MSGSUP = 32000
         CALL QINIT (NEED, 0, KAP)
         MSGSUP = 0
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = (NY * (NX/2 + 1)) / 2
            NEED = NEED / 1024
            NEED = MIN (16 * 1024, NEED) + 4
            CALL QINIT (NEED, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               MSGTXT = 'CANNOT GET NEEDED MEMORY'
               IERR = 8
               GO TO 990
               END IF
            END IF
         IDIR = 3
         CALL DSKFFT (NX, NY, IDIR, T, SCRNM(WK1), SCRNM(WK3),
     *     SCRNM(WT(IFIELD)), BUFSZ(1), BUFFR1, BUFFR2, RMAX,
     *     RMIN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1220) IERR
            GO TO 990
            END IF
         CALL QRLSE
C                                       Find volume of beam
         CALL FNDVAR (IFIELD, WT(IFIELD), IERR)
         IF (IERR.NE.0) GO TO 999
 50      CONTINUE
C                                       End of beam loop
C
C                                       Loop over dirty maps
C                                       Get catalog slot number for
C                                       the dirty map and it's CATBLK.
      DO 60 IFIELD = 1,NFIELD
         CNO = 1
         ITMP = SEQ(1) + IFIELD - 1
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', VOL(1), CNO, NAME(1), CLASS(1), ITMP,
     *      MTYPE, USID, STAT, BUFFR3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, NAME(1), CLASS(1), ITMP, VOL(1),
     *         USID
            GO TO 990
            END IF
C                                       Get CATBLK for dirty map.
         CALL CATIO ('READ', VOL(1), CNO, CATBLK, 'READ', BUFFR3,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 990
            END IF
C                                       Mark map READ in /CFILES/
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL(1)
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 0
C                                       Warn if "UTESS"
         IF (CATBLK(KINIT).GT.0) THEN
            WRITE (MSGTXT,1100)
            CALL MSGWRT (4)
            END IF
C                                       Find type of telescope
C                                       Default type = Gaussian
C                                       VLA if in header
C                                       Single dish if BMSIZE(IFIELD) <
C                                       0
         IF (BMSIZE(IFIELD).LT.0.0) THEN
            BEAMTP(IFIELD) = 0
         ELSE IF (BMSIZE(IFIELD).GT.0.0) THEN
            BEAMTP(IFIELD) = 1
         ELSE
            CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), LF,
     *         IERR)
            IF (IERR.EQ.0) THEN
               FREQ = CATD(KDCRV+LF)
            ELSE
               FREQ = 0.0D0
               END IF
            BEAMTP(IFIELD) = 2
            IF (FREQ.NE.0.) THEN
               BMSIZE(IFIELD) = VELITE / FREQ
            ELSE
               BMSIZE(IFIELD) = 0.0
               END IF
            END IF
C                                       Find pointing center
         IDEPTH(1) = CHAN
         LOCNUM = 1
         CALL SETLOC (IDEPTH, F)
         CALL XYPIX (CATD(KDORA), CATD(KDODE), RCENX(IFIELD),
     *      RCENY(IFIELD), JERR)
         IF (JERR.NE.0) THEN
            RCENX(IFIELD) = CATR(KRCRP)
            RCENY(IFIELD) = CATR(KRCRP+1)
            END IF
C
         VMVOL(DAT(IFIELD)) = VOL(1)
         SCRNM(DAT(IFIELD)) = SCRNM(CVMOUT) + IFIELD
         SCRCNO(SCRNM(DAT(IFIELD))) = CNO
         CALL ZPHFIL ('MA', VOL(1), CNO, 1, VMFILE(DAT(IFIELD)), IERR)
C                                       Set up block offsets for reading
         CORN(3) = CHAN
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3),
     *      VMBO(DAT(IFIELD)), IERR)
         VMBO(DAT(IFIELD)) = VMBO(DAT(IFIELD)) + 1
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       save coordinates
         IF (IFIELD.EQ.1) THEN
            MCRV(1) = CATD(KDCRV)
            MCIC(1) = CATR(KRCIC)
            MCRP(1) = CATR(KRCRP)
            MNAX(1) = CATBLK(KINAX)
            MCRV(2) = CATD(KDCRV+1)
            MCIC(2) = CATR(KRCIC+1)
            MCRP(2) = CATR(KRCRP+1)
            MNAX(2) = CATBLK(KINAX+1)
            MEPS = 0.01 * MAX (ABS(MCIC(1)), ABS(MCIC(2)))
C                                       test
         ELSE
            IF ((ABS(MCRV(1)-CATD(KDCRV)).GT.MEPS) .OR.
     *         (ABS(MCRV(2)-CATD(KDCRV+1)).GT.MEPS) .OR.
     *         (ABS(MCIC(1)-CATR(KRCIC)).GT.0.01*MEPS) .OR.
     *         (ABS(MCIC(2)-CATR(KRCIC+1)).GT.0.01*MEPS) .OR.
     *         (ABS(MCRP(1)-CATR(KRCRP)).GT.0.1) .OR.
     *         (ABS(MCRP(2)-CATR(KRCRP+1)).GT.0.1) .OR.
     *         (MNAX(1)-CATBLK(KINAX).NE.0) .OR.
     *         (MNAX(2)-CATBLK(KINAX+1).NE.0)) THEN
               WRITE (MSGTXT,1050) IFIELD
               CALL MSGWRT (7)
               NOMAT = NOMAT + 1
               END IF
            END IF
 60      CONTINUE
      IF (NOMAT.GT.0) THEN
         WRITE (MSGTXT,1060) NOMAT
         IERR =  NOMAT
         GO TO 990
         END IF
C                                       End of loop over dirty maps
C
C                                       Make sure NX,NY same as
C                                       for the beam.
      IF ((NX.NE.CATBLK(KINAX)) .OR. (NY.NE.CATBLK(KINAX+1)))
     *   THEN
         IERR = 1
         WRITE (MSGTXT,1105)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1106) NX,NY,CATBLK(KINAX),CATBLK(KINAX+1)
         GO TO 990
         END IF
C                                       Create VM file if necessary
C                                       Put NAME in CATBLK
      IF (NAME(3).NE.'            ')
     *   CALL CHR2H (12, NAME(3), KHIMNO, CATH(KHIMN))
C                                       Put VM class into CATBLK
      CALL CHR2H (6, CLASS(3), KHIMCO, CATH(KHIMC))
C                                        Put in units of map
      CALL CHR2H (8, MAPUNT, 1, CATH(KHBUN))
C                                       Update sequence.
      CATBLK(KIIMS) = SEQ(3)
      CATR(KRDMX) =  0.0
      CATR(KRDMN) =  0.0
C                                       Create output VM map file.
      CALL MCREAT (VOL(3), CNO, BUFFR1, IERR)
      OLD = .FALSE.
      IF (IERR.NE.0) THEN
         OLD = .TRUE.
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1190) IERR
            GO TO 990
         ELSE
C                                       Existing file: check it out
            OLDIM = CATBLK(KIDIM)
            DO 70 I = 1,7
               OLDNAX(I) = CATBLK(KINAX+I-1)
               OLDCRV(I) = CATD(KDCRV+I-1)
 70           CONTINUE
            CALL CATIO ('READ', VOL(3), CNO, CATBLK, 'REST', BUFFR3,
     *         IERR)
            IF (OLDIM.NE.CATBLK(KIDIM)) GO TO 80
            IMGMAX = CATR(KRDMX)
            IMGMIN = CATR(KRDMN)
            DO 90 I = 1,2
               IF (OLDNAX(I).NE.CATBLK(KINAX+I-1)) GO TO 80
               IF (OLDCRV(I).NE.CATD(KDCRV+I-1)) GO TO 80
 90           CONTINUE
C                                       mark WRITE
            CALL CATIO ('WRIT', VOL(3), CNO, CATBLK, 'WRIT', BUFFR3,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1210) IERR
               CALL MSGWRT (8)
               END IF
            GO TO 100
 80         CONTINUE
            IERR = 8
            WRITE (MSGTXT,1215)
            GO TO 990
           END IF
         END IF
 100  CONTINUE
C
      CORN(3) = CHAN
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), VMBO(VMOUT),
     *   IERR)
      VMBO(VMOUT) = VMBO(VMOUT) + 1
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1045) IERR
         GO TO 990
         END IF
C                                       Actual seq #
      SEQ(3) = CATBLK(KIIMS)
C                                        Get old ALPHA
C   potential problem here
      I = 255
      ALPINI = CATR(I)
C                                        Mark for READ in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(3)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Fill output file into common
      VMVOL(VMOUT) = VOL(3)
      SCRCNO(SCRNM(VMOUT)) = CNO
      CALL ZPHFIL ('MA', VOL(3), CNO, 1, VMFILE(VMOUT), IERR)
C                                       Now make or get second output
C                                       image
      IF (BMAJ.GE.0.0) THEN
         CALL COPY (256, CATBLK, CATIMG)
C                                       Create file if necessary
C                                       Put NAME in CATBLK
         CALL CHR2H (12, NAME(5), KHIMNO, CATH(KHIMN))
C                                       Put class into CATBLK
         CALL CHR2H (6, CLASS(5), KHIMCO, CATH(KHIMC))
C                                       Update sequence.
         CATBLK(KIIMS) = SEQ(5)
C                                        Put in units of map
         CALL CHR2H (8, CMPUNT, 1, CATH(KHBUN))
C
         IF (BLANKD) THEN
            CATR(KRBLK) = INDEF
         ELSE
            CATR(KRBLK) = 0.0
            END IF
         CATR(KRDMX) = -1E10
         CATR(KRDMN) =  1E10
C                                       Create second output map file.
         CALL MCREAT (VOL(5), CNO, BUFFR1, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.2) THEN
C                                       Existing file: check it out
               OLDIM = CATBLK(KIDIM)
               DO 120 I = 1,7
                  OLDNAX(I) = CATBLK(KINAX+I-1)
                  OLDCRV(I) = CATD(KDCRV+I-1)
 120           CONTINUE
               CALL CATIO ('READ', VOL(5), CNO, CATBLK, 'REST',
     *            BUFFR3, IERR)
               IF (OLDIM.NE.CATBLK(KIDIM)) GO TO 130
               DO 140 I = 1,2
                  IF (OLDNAX(I).NE.CATBLK(KINAX+I-1)) GO TO 130
                  IF (OLDCRV(I).NE.CATD(KDCRV+I-1)) GO TO 130
 140              CONTINUE
C                                       mark WRITE
               CALL CATIO ('WRIT', VOL(5), CNO, CATBLK, 'WRIT',
     *            BUFFR3, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1210) IERR
                  CALL MSGWRT (8)
                  END IF
               GO TO 150
 130           CONTINUE
               IERR = 8
               WRITE (MSGTXT,1515)
               GO TO 990
            ELSE
               WRITE (MSGTXT,1190) IERR
               GO TO 990
               END IF
            END IF
C                                       Actual seq #
 150     SEQ(5) = CATBLK(KIIMS)
C
         CORN(3) = CHAN
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3),
     *      VMBO(CVMOUT), IERR)
         VMBO(CVMOUT) = VMBO(CVMOUT) + 1
C                                        Mark for WRIT in /CFILES/
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL(5)
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 1
C                                       Fill output file into common
         VMVOL(CVMOUT) = VOL(5)
         SCRCNO(SCRNM(CVMOUT)) = CNO
         CALL ZPHFIL ('MA', VOL(5), CNO, 1, VMFILE(CVMOUT), IERR)
         CALL COPY (256, CATIMG, CATBLK)
         END IF
C                                       GET Default level file
C                                       if it exists
      IF (DEFEXT) THEN
         CALL COPY (256, CATBLK, CATIMG)
         CNO = 1
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', VOL(4), CNO, NAME(4), CLASS(4), SEQ(4),
     *      MTYPE, USID, STAT, BUFFR3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, NAME(4), CLASS(4), SEQ(4), VOL(4),
     *         USID
            GO TO 990
            END IF
         CALL CATIO ('READ', VOL(4), CNO, CATDEF, 'READ', BUFFR3,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 990
            END IF
C
         CORN(3) = 1
         WIN(1) = BLC(1)
         WIN(2) = BLC(2)
         WIN(3) = TRC(1)
         WIN(4) = TRC(2)
         CALL PLNGET (VOL(4), CNO, CORN, WIN, XOFF, YOFF, SCRNM(DEF),
     *      VMSZ(1,DEF), VMSZ(2,DEF), BUFFR1, BUFFR2, BUFSZ(1),
     *      BUFSZ(2), LUN1, LUN2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1045) IERR
            GO TO 990
            END IF
         DEFMAX = CATR(KRDMX)
         DEFMIN = CATR(KRDMN)
C                                       Add to /CFILES/, Mark READ
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL(4)
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 0
C
         CALL COPY (256, CATIMG, CATBLK)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILES: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I4)
 1010 FORMAT ('FILES: CANNOT COPY BEAM CATBLK, ERROR',I3)
 1020 FORMAT ('FILES: ERROR',I3,' CREATING SCRATCH FILE ',I2)
 1035 FORMAT ('FILES: CANNOT COPY DIRTY BEAM, ERROR ',I3)
 1040 FORMAT ('FILES: CANNOT COPY DIRTY MAP, ERROR ',I3)
 1045 FORMAT ('FILES: CANNOT COPY UTESS MAP, ERROR ',I3)
 1050 FORMAT ('FILES: FIELD',I5,' DOES NOT MATCH GEOMETRY OF FIELD 1')
 1060 FORMAT ('FILES:',I5,' FIELDS DO NOT MATCH: USE HGEOM')
 1090 FORMAT ('FILES: CANNOT COPY MAP CATBLK, ERROR',I3)
 1100 FORMAT ('WARNING: MAY BE UTESSING A UTESS MAP')
 1105 FORMAT ('FILES: UNEQUAL DIMENSIONS IN DIRTY AND BEAM MAPS')
 1106 FORMAT ('       BEAM =',2I5,' DIRTY =',2I5)
 1190 FORMAT ('FILES: COULD NOT CREATE UTESS MAP FILE, ERROR ',I3)
 1210 FORMAT ('FILES: CANNOT UPDATE UTESS CATBLK, ERROR',I3)
 1215 FORMAT ('OLD UTESS MAP NOT COMPATIBLE WITH DIRTY MAP')
 1220 FORMAT ('FILES: ERROR ',I3, ' IN DSKFFT, BEAM TO U,V')
 1515 FORMAT ('OLD RESTORED MAP NOT COMPATIBLE WITH DIRTY MAP')
      END
      SUBROUTINE GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IERR)
C-----------------------------------------------------------------------
C   GETIN gets the input parameters for the program from AIPS
C   initializes the parameters, and sets up the file system.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DTESS.INC'
C
      CHARACTER PRGNAM*6, NAME(5)*12, CLASS(5)*6, MTYPE*2
      HOLLERITH XNAM1(3), XNAM2(3), XNAM3(3), XNAM4(3), XNAM5(3),
     *   XCLAS1(2), XCLAS2(2), XCLAS3(2), XCLAS4(2), XCLAS5(2)
      INTEGER   USID, SEQ(5), VOL(5), I, LUN1, ITV
      INTEGER   NPARMS, CATBLK(256), IND, CNO
      INTEGER   IROUND
      REAL      XSEQ1, XSEQ2, XSEQ3, XSEQ4, XSEQ5, XVOL1, XVOL2, XVOL3,
     *   XVOL4, XVOL5, XNITER, XERROR(64), XFLUX, XGAIN, XNPPB, XNFIEL,
     *   XBLC(7), XTRC(7), XLNGO, XDOTV, XBMAJ, XBMIN, XBPA, XBMSIZ(64),
     *   XAPM(7), XBAD(10)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XVOL1,
     *   XNAM2, XCLAS2, XSEQ2, XVOL2,
     *   XNAM4, XCLAS4, XSEQ4, XVOL4,
     *   XNAM3, XCLAS3, XSEQ3, XVOL3,
     *   XNAM5, XCLAS5, XSEQ5, XVOL5,
     *   XNFIEL, XNITER, XERROR, XFLUX,
     *   XBLC, XTRC, XDOTV, XLNGO, XBMSIZ, XBMAJ, XBMIN, XBPA, XAPM,
     *   XBAD
      DATA PRGNAM /'UTESS '/
C-----------------------------------------------------------------------
C
      IERR = 0
      LUN1 = 16
C                                        Initialize common parameters.
C                                        global areas
      CALL ZDCHIN (.TRUE., BUFFR1)
      CALL VHDRIN
      CALL HIINIT (5)
      BLC(1) = 0
      BLC(2) = 0
      TRC(1) = 0
      TRC(2) = 0
      VMLIM = 5
      VMSTR = 0
      GAIN = 0.30
      SCLF = 1.0
      EPSSTR = 1.0
      FLUX = 0.0
      BUFSZ(1) = XBUFSZ * 2
      BUFSZ(2) = XBUFSZ * 2
      BUFSZ(3) = XBUFSZ * 2
      BUFSZ(4) = XBUFSZ * 2
      BUFSZ(5) = XBUFSZ * 2
      BUFSZ(6) = XBUFSZ * 2
      BPS = NBPS
C                                       Get AIPS adverbs.
      NPARMS = 5*7 + 2*1 + 64 + 1 + 2*7 + 2*1 + 64 + 3*1 + 7 + 10
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, BUFFR1, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
C                                       Convert characters
 10   CALL H2CHR (12, 1, XNAM1, NAME(1))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (12, 1, XNAM2, NAME(2))
      CALL H2CHR (6, 1, XCLAS2, CLASS(2))
      CALL H2CHR (12, 1, XNAM3, NAME(3))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
      CALL H2CHR (12, 1, XNAM4, NAME(4))
      CALL H2CHR (6, 1, XCLAS4, CLASS(4))
      CALL H2CHR (12, 1, XNAM5, NAME(5))
      CALL H2CHR (6, 1, XCLAS5, CLASS(5))
C                                       get actual dirty map name
      VOL(1) = IROUND (XVOL1)
      SEQ(1) = IROUND (XSEQ1)
      USID = NLUSER
      MTYPE = 'MA'
      CALL MAPOPN ('READ', VOL, NAME(1), CLASS(1), SEQ, MTYPE, USID,
     *   LUN1, IND, CNO, CATBLK, BUFFR1, IERR)
      DRTCNO = CNO
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR,'READ'
         CALL MSGWRT (8)
         GO TO 999
 20   CALL MAPCLS ('READ', VOL, CNO, LUN1, IND, CATBLK, F, BUFFR1,
     *   IERR)
      IF (IERR.EQ.0) GO TO 25
         WRITE (MSGTXT,1020) IERR, 'READ'
         CALL MSGWRT (6)
 25   IERR = 0
C                                       Default name for Beam and VM
C                                       maps is Dirty map name.
      IF (NAME(2) .EQ. ' ') NAME(2) = NAME(1)
      IF (NAME(3) .EQ. ' ') NAME(3) = NAME(1)
      IF (NAME(5) .EQ. ' ') NAME(5) = NAME(1)
C                                       Copy CLASSes or default.
      IF (CLASS(2) .EQ. ' ') THEN
         CLASS(2) = 'IBEAM '
         IF (CLASS(1).EQ.'RMAP') CLASS(2) = 'RBEAM '
         IF (CLASS(1).EQ.'LMAP') CLASS(2) = 'LBEAM '
         END IF
C
      IF (CLASS(3).EQ.' ') THEN
         IF (INDEX (CLASS(1), 'MAP') .EQ. 2) THEN
            CLASS(3)(1:1) = CLASS(1)(1:1)
            CLASS(3)(2:)  = 'UT   '
         ELSE
            CLASS(3) = 'UT'
            END IF
         END IF
C
      IF (CLASS(5).EQ.' ') THEN
         IF (INDEX (CLASS(1), 'MAP') .EQ. 2) THEN
            CLASS(5)(1:1) = CLASS(1)(1:1)
            CLASS(5)(2:)  = 'UTC  '
         ELSE
            CLASS(5) = 'UTC   '
            END IF
         END IF
C                                       Def. Level file must be
C                                       specified completely
      DEFEXT = .FALSE.
      IF (NAME(4).EQ.' ') GO TO 45
         IF (CLASS(4).EQ.' ') GO TO 45
            VOL(4) = IROUND (XVOL4)
            IF (VOL(4).EQ.0) GO TO 45
               SEQ(4) = IROUND (XSEQ4)
               IF (SEQ(4).EQ.0) GO TO 45
                  DEFEXT = .TRUE.
C                                       Get volume numbers
 45   VOL(2) = IROUND (XVOL2)
      VOL(3) = IROUND (XVOL3)
      VOL(5) = IROUND (XVOL5)
C                                       Get sequence numbers
      SEQ(2) = IROUND (XSEQ2)
      SEQ(3) = IROUND (XSEQ3)
      SEQ(5) = IROUND (XSEQ5)
      IF (SEQ(2).LE.0) SEQ(2) = SEQ(1)
C
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 70      CONTINUE
C                                       Get UTESS information
      BLC(1) = IROUND (MIN (XBLC(1), XTRC(1)))
      BLC(2) = IROUND (MIN (XBLC(2), XTRC(2)))
      TRC(1) = IROUND (MAX (XBLC(1), XTRC(1)))
      TRC(2) = IROUND (MAX (XBLC(2), XTRC(2)))
      CHAN = MAX (1.0, XBLC(3)+0.1)
      NFIELD = MIN (NINT (XNFIEL), MATFLD)
      IF (NFIELD.LT.1) NFIELD = 1
      TFLUX = XFLUX
      XNPPB = (XBAD(9) - IBAD(9))*1000
      RNPPB = MAX (XNPPB, 0.0)
      IF (XNPPB.EQ.0.0) RNPPB = 1.0
      XGAIN = XBAD(10) - IBAD(10)
      IF (XGAIN.NE.0.0) GAIN = XGAIN
      DOSTOP = XNITER.LT.0.0
      IF (XNITER.NE.0.0) VMLIM = ABS(XNITER) + 0.1
      ITV = XDOTV
      BMAJ = XBMAJ
      BMIN = XBMIN
      BPA = XBPA
      BLANKD = F
      DO 666 I = 1,NFIELD
         IF (I.LE.64) THEN
            FRES(I) = XERROR(I)
            BMSIZE(I) = XBMSIZ(I)
         ELSE
            FRES(I) = XERROR(64)
            BMSIZE(I) = XBMSIZ(64)
            END IF
         IF (BMSIZE(I).GE.0.0) BLANKD = T
C                                       NOISE must be specified
         IF (FRES(I).GT.0.0) GO TO 666
            IERR = 8
            WRITE(MSGTXT,1005) I
            CALL MSGWRT (8)
            GO TO 999
 666     CONTINUE
C                                       cutoff for primary beam
      IF (XAPM(1).LE.0.0) XAPM(1) = 0.07
      CALL RCOPY (7, XAPM, PBPARM)
C                                       Get bad disks.
      LNGOUT = .FALSE.
      IF (XLNGO.GT.0.0) LNGOUT = .TRUE.
C                                       Check if TV allowed
      IF (NTVDEV.LE.0) ITV = 0
C
C                                       UTESS common areas
      DO 778 I = 1,NFIELD
         WT(I)  = I
 778     CONTINUE
      DEF = WT(NFIELD) + 1
      IMG = WT(NFIELD) + 2
      DEL = WT(NFIELD) + 3
      RES = WT(NFIELD) + 4
      SCR = WT(NFIELD) + 5
      WK1 = WT(NFIELD) + 6
      WK2 = WT(NFIELD) + 7
      WK3 = WT(NFIELD) + 8
      PRS = WT(NFIELD) + 9
      NUMSCR = PRS
C
      DO 779 I=1,NFIELD
         DAT(I)  = NUMSCR + I
  779    CONTINUE
      VMOUT = DAT(NFIELD) + 1
      CVMOUT = VMOUT + 1
      NUMFIL = CVMOUT
C                                       Output files go in common.
      WRITE (MSGTXT,1200) NUMSCR
      CALL MSGWRT (4)
      DO 125 I = 1,NUMFIL
         VMBO(I) = 1
 125  CONTINUE
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1005 FORMAT ('GETIN: NOISE has to be specified for field #',I2)
 1010 FORMAT ('GETIN: ERROR',I3,' OPENING DIRTY MAP FILE FOR ',A4)
 1020 FORMAT ('GETIN: ERROR',I3,' CLOSING DIRTY MAP FILE FOR ',A4)
 1200 FORMAT ('Creating ',I3,' scratch files')
      END
      SUBROUTINE VMHIS (NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   VMHIS copies the dirty map history if the VM map does not
C   already have a history file.  Then the inputs to VM with
C   the default values are added to the history file.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      CHARACTER NAME(5)*12, CLASS(5)*6, BEAMNM(2)*8, ADATE*12, ATIME*8
      INTEGER   VOL(5), SEQ(5), CATBLK(256), LUN1, LUN2, ITMDAT(6),
     *   IFIELD
      LOGICAL   OLD
      CHARACTER HILINE*72
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      DATA BEAMNM /'Gaussian','VLA     '/
C-----------------------------------------------------------------------
      IERR = 0
      LUN1 = 16
      LUN2 = 17
C                                       Open old file: no copy old HI
      IF (.NOT.OLD) GO TO 20
         CALL HIOPEN (LUN1, VMVOL(VMOUT), SCRCNO(SCRNM(VMOUT)),
     *    BUFFR2, IERR)
         IF (IERR.EQ.5) GO TO 20
         IF (IERR.NE.0) GO TO 10
            CALL ZDATE (ITMDAT(1))
            CALL ZTIME (ITMDAT(4))
            CALL TIMDAT (ITMDAT(4), ITMDAT(1), ATIME, ADATE)
            WRITE (HILINE,1000) TSKNAM, RLSNAM, ADATE, ATIME
            CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
            IF (IERR.NE.0) GO TO 50
            GO TO 30
 10      CONTINUE
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 60
C                                       Copy/open history files.
 20   CONTINUE
         CALL KEYPCP (VOL(1), SCRCNO(SCRNM(DAT(1))), VOL(3),
     *       SCRCNO(SCRNM(VMOUT)), 0, ' ', IERR)
         CALL HISCOP (LUN2, LUN1, VOL(1), VOL(3), SCRCNO(SCRNM(DAT(1))),
     *       SCRCNO(SCRNM(VMOUT)), CATBLK, BUFFR1, BUFFR2, IERR)
         IF (IERR.EQ.0) GO TO 30
C                                       Check if old file doesn't exist
         IF (IERR.EQ.2) GO TO 30
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (6)
            GO TO 60
C                                       Add to history file.
C                                       Dirty map name.
 30   WRITE (MSGTXT,2000) NAME(1), CLASS(1),SEQ(1),VOL(1)
      CALL MSGWRT (3)
      CALL HENCO1 (TSKNAM, NAME(1), CLASS(1), SEQ(1), VOL(1),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Dirty beam name.
      WRITE (MSGTXT,2001) NAME(2), CLASS(2),SEQ(2),VOL(2)
      CALL MSGWRT (3)
      CALL HENCO2 (TSKNAM, NAME(2), CLASS(2), SEQ(2), VOL(2),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       UTESS map name.
      WRITE (MSGTXT,2002) NAME(3), CLASS(3), SEQ(3), VOL(3)
      CALL MSGWRT (3)
      CALL HENCOO (TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                      Restored map name.
      IF (BMAJ.GT.0.0) THEN
         WRITE (MSGTXT,2032) NAME(5), CLASS(5), SEQ(5), VOL(5)
         CALL MSGWRT (3)
         CALL HENCOO (TSKNAM, NAME(5), CLASS(5), SEQ(5), VOL(5),
     *      LUN1, BUFFR2, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       DEF LEVEL map name.
      IF (.NOT.DEFEXT) GO TO 45
         WRITE (MSGTXT,2222) NAME(4), CLASS(4), SEQ(4), VOL(4)
         CALL MSGWRT (3)
         CALL HENCO3 (TSKNAM, NAME(4), CLASS(4), SEQ(4), VOL(4),
     *      LUN1, BUFFR2, IERR)
         IF (IERR.NE.0) GO TO 50
C                                       Maximum number of Iterations.
 45   WRITE (HILINE,2003) TSKNAM, VMLIM
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Start iteration.
      WRITE (HILINE,2004) TSKNAM, VMSTR
      IF (VMSTR.GT.0) CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      IF (VMSTR.GT.0) CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Total FLUX
      WRITE (HILINE,2007) TSKNAM, TFLUX
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Boxes
      WRITE (HILINE,2009) TSKNAM, BLC(1), BLC(2)
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Boxes
      WRITE (HILINE,2010) TSKNAM, TRC(1),TRC(2)
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Pointing centers
      DO 777 IFIELD=1,NFIELD
         WRITE (HILINE,2023) TSKNAM, IFIELD, IFIELD,
     *      RCENX(IFIELD), RCENY(IFIELD)
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
  777    CONTINUE
C                                       Noise level
      DO 779 IFIELD=1,NFIELD
         WRITE (HILINE,2006) TSKNAM, IFIELD, FRES(IFIELD)
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
  779    CONTINUE
C                                       Pointing centers
      DO 780 IFIELD=1,NFIELD
         IF (BEAMTP(IFIELD).EQ.0) THEN
            WRITE (HILINE,2026) TSKNAM, IFIELD
            CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            IF (IERR.NE.0) GO TO 50
            IF (BMSIZE(IFIELD).LE.0.0) THEN
               WRITE (HILINE,2027) TSKNAM, IFIELD
               CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
               MSGTXT = HILINE(7:)
               CALL MSGWRT (2)
               IF (IERR.NE.0) GO TO 50
               END IF
         ELSE
            WRITE (HILINE,2028) TSKNAM, IFIELD
            CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            IF (IERR.NE.0) GO TO 50
            IF (BMSIZE(IFIELD).LE.0.0) THEN
               WRITE (HILINE,2027) TSKNAM, IFIELD
               CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
               MSGTXT = HILINE(7:)
               CALL MSGWRT (2)
               IF (IERR.NE.0) GO TO 50
            ELSE
               WRITE (HILINE,2025) TSKNAM, IFIELD,
     *            BEAMNM(BEAMTP(IFIELD))
               CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
               MSGTXT = HILINE(7:)
               CALL MSGWRT (2)
               IF (IERR.NE.0) GO TO 50
               IF (BEAMTP(IFIELD).EQ.1) THEN
                  WRITE (HILINE,2024) TSKNAM, IFIELD, BMSIZE(IFIELD)
               ELSE
                  WRITE (HILINE,2022) TSKNAM, IFIELD, BMSIZE(IFIELD)
                  END IF
               CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
               MSGTXT = HILINE(7:)
               CALL MSGWRT (2)
               IF (IERR.NE.0) GO TO 50
               END IF
            END IF
  780    CONTINUE
C                                       Q per field
      DO 781 IFIELD=1,NFIELD
         WRITE (HILINE,2008) TSKNAM, IFIELD, RVAR(IFIELD)
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
  781    CONTINUE
C                                       CHAN
      WRITE (HILINE,2177) TSKNAM, CHAN
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       BMAJ, BMIN, BPA
      IF (BMAJ.GT.0.0) THEN
         WRITE (HILINE,2178) TSKNAM, BMAJ
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2179) TSKNAM, BMIN
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2180) TSKNAM, BPA
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
         END IF
C
      GO TO 60
C                                       Error has occured.
 50   WRITE (MSGTXT,1050) IERR
      CALL MSGWRT (6)
C                                       Close history files.
 60   CALL HICLOS (LUN1, T, BUFFR2, IERR)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /******** Restart ',
     *   A12,1X,A8)
 1010 FORMAT ('UNABLE TO OPEN OLD UTESS-MAP HISTORY FILE')
 1020 FORMAT ('VMHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT ('VMHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT ('Dirty map =    ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2001 FORMAT ('Beam map =     ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2002 FORMAT ('Utess map =    ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2032 FORMAT ('Restored map = ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2222 FORMAT ('Default map =  ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2003 FORMAT (A6,'NITER = ',I6,' /Max. no. UTESS iterations')
 2004 FORMAT (A6,'BITER = ',I6,' /No. of previous UTESS iteration')
 2007 FORMAT (A6,'FLUX = ',F8.3,' /Required total flux')
 2009 FORMAT (A6,'BLC = ',2(I5,1X),' /Bottom left corner')
 2010 FORMAT (A6,'TRC = ',2(I5,1X),' /Top   right corner')
 2177 FORMAT (A6,'BLC(3) = ',I6,' /Channel number')
 2022 FORMAT (A6,'/Field',I5,' VLA lambda=',F7.4,' m for primary beam')
 2023 FORMAT (A6,'PBCENX(',I4,'), PBCENY(',I4,') = ',F7.2,',',
     *   F7.2,' /Pixels')
 2024 FORMAT (A6,'PBSIZE(',I4,') = ',F8.1,'/ Arcseconds')
 2025 FORMAT (A6,'/Field',I5,' : Primary beam = ',A8)
 2026 FORMAT (A6,'/Field',I5,' : Single aperture data (?)')
 2028 FORMAT (A6,'/Field',I5,' : Interferometer  data')
 2027 FORMAT (A6,'/Field',I5,' : No primary beam correction')
 2006 FORMAT (A6,'NOISE(',I4,') = ',1PE11.3,'/ Jy/beam')
 2008 FORMAT (A6,'NPOINTS(',I4,') = ',1PE11.3,'/ Pixels/beam')
 2178 FORMAT (A6,'BMAJ = ',1PE11.3,' / Beam major axis (asec)')
 2179 FORMAT (A6,'BMIN = ',1PE11.3,' / Beam minor axis (asec)')
 2180 FORMAT (A6,'BPA  = ',1PE11.3,' / Beam p.a. (degrees)')
      END
      SUBROUTINE DISPTV (IN, TMAX, TMIN, TVPASS)
C-----------------------------------------------------------------------
C   DISPTV displays the FILE IN on the TV
C   Inputs:
C      TVPASS  I  code: ABS=1 => clear screen, else don't
C                          <0 => don't question the user about
C                                quitting
C   Output:
C      TVPASS  I  code: 32700 => user wants to quit
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      CHARACTER IBLANK*2
      INTEGER   IN, TVPASS, JROW(1), WIN(4), MY, FND, BND, ICH, INC(2),
     *   CATBLK(256), CATITM(256), IQ, IB, I, LUN, MX2, MX, IWIN(4), MV
      REAL      XN(4), XBUFF(1), CATR(256), TD, RPOS(2)
      REAL      TMIN, TMAX
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (JROW(1), BUFFR2),    (BUFFR1, XBUFF(1))
      EQUIVALENCE (CATITM, CATR, CATD, BUFFR3)
      DATA IBLANK /'  '/
C-----------------------------------------------------------------------
      LUN = 24
      IF (TMIN.GE.TMAX) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (4)
         GO TO 999
         END IF
      ICH = 1
      CALL TVOPEN (BUFFR1, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ABS (TVPASS).EQ.1) THEN
         CALL TVSET (ICH, BUFFR1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 998
            END IF
         END IF
      XN(1) = TMIN
      XN(2) = TMAX
      XN(3) = (MAXINT - 0.02) / (TMAX-TMIN)
      XN(4) = 0.51 - TMIN*XN(3)
      CALL QPUT (XN, 0, 4, 2)
C
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = VMSZ(1,IN)
      WIN(4) = VMSZ(2,IN)
      INC(1) = VMSZ(1,IN) / MAXXTV(1) + 1
      INC(2) = VMSZ(2,IN) / MAXXTV(2) + 1
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
      CALL ZOPEN (LUN, FND, VMVOL(IN), VMFILE(IN), MAP, EXCL, WAIT,
     *   IERR)
      IF (IERR.NE.0) GO TO 115
      CALL MINIT ('READ', LUN, FND, VMSZ(1,IN), VMSZ(2,IN), WIN, XBUFF,
     *   BUFSZ(1), VMBO(IN), IERR)
      IF (IERR.NE.0) GO TO 110
      MX = WIN(3) - WIN(1) + 1
      MY = WIN(4) - WIN(2) + 1
      MV = IWIN(3) - IWIN(1) + 1
      MX2 = MV

C                                       loop, passing map to TV.
      IY = IWIN(2) - 1
      DO 100 I = 1,MY
         CALL MDISK ('READ', LUN, FND, XBUFF, BND, IERR)
         IF (IERR.NE.0) GO TO 110
         IF (MOD(I-1, INC(2)).EQ.0) THEN
            CALL QPUTIN (XBUFF(BND), INC(1), 4, 1, MV, 2)
            CALL QWD
            CALL QVCLIP (4, 1, 0, 1, 4, 1, MV)
            CALL QVSMSA (4, 1, 2, 3, 4, 1, MV)
            CALL QVFIX (4, 1, 4, 1, MV)
            CALL QWR
            CALL QGET (JROW, 4, MV, 1)
            CALL QWD
            IY = IY + 1
            CALL YIMGIO ('WRIT', ICH, IWIN(1), IY, 0, MX2, JROW, IERR)
            IF (IERR.NE.0) GO TO 110
            END IF
 100     CONTINUE
 110  CALL ZCLOSE (LUN, FND, IERR)
C                                       Image catalog
 115  CALL COPY (256, CATBLK, CATITM)
      CATITM(IIVOL) = 0
      CATITM(IICNO) = 0
      CALL FILL (5, 1, CATITM(IIDEP))
      CALL COPY (4, IWIN, CATITM(IICOR))
      CALL COPY (4, WIN, CATITM(IIWIN))
      CALL CHR2H (2, IBLANK, 1, CATITM(IITRA))
      CATR(IRRAN) = TMIN
      CATR(IRRAN+1) = TMAX
      CATR(KRDMN) = TMIN
      CATR(KRDMX) = TMAX
      CALL YCWRIT (ICH, IWIN, CATITM, XBUFF, IERR)
      IF (IERR.EQ.0) GO TO 120
         WRITE (MSGTXT,1110)
         CALL MSGWRT (6)
C                                       Ask user to quit?
 120  IF (TVPASS.LT.0) GO TO 998
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
            IF (IB.GT.7) GO TO 140
            IF (IB.GT.0) GO TO 135
            IF (IERR.NE.0) GO TO 150
 130        CONTINUE
 135     MSGTXT = 'Continuing'
         CALL MSGWRT (1)
         GO TO 150
C                                       Wants to quit
 140     TVPASS = 32700
         MSGTXT = 'TV Button D hit: have done enough I guess'
         CALL MSGWRT (3)
C                                       Off cursor
 150     CALL YCURSE ('OFFF', F, F, RPOS, IQ, IB, IERR)
C
 998  CALL TVCLOS (BUFFR1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MIN GREATER OR EQUAL TO MAX')
 1010 FORMAT ('IMCLEAR ERROR =',I6)
 1070 FORMAT ('Loading TV from',1PE10.3,' to',1PE10.3,' every',I2,
     *   ' pixel')
 1110 FORMAT ('CAN''T UPDATE IMAGE CATALOG  IER=',I6)
      END
