LOCAL INCLUDE 'APVC.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for APVC
      DOUBLE PRECISION PTRJ, PTRK
      REAL      SCRAT(6), GAIN, FLUX, MAPLIM, GAUSA, GAUSB, GAUSC,
     *   GAUSAA, GAUSBB, GAUSCC, ALPHA, XSPACE, YSPACE, MAPROT, SPEXP,
     *   TVFMAX, APARM(10), PHAT, RESMAX, RESMIN, RESRMS, HATMAX,
     *   HATMIN, BUFF1(MABFSS), BUFF2(MABFSS), BUFF3(MABFSS)
      INTEGER   NX, NY, NUMBIN, VCLST, VCSTR, NBOXS, WINM(4,10), VCLIM,
     *   BPS, LUNBEM, WINB(4), LUNHAT, LUNRES, LUNRS1, LUNWT, LUNGD1,
     *   LUNGD2, LUNDRT, LUNWRK, LUNCL1, LUNCL2, BEMVOL, RESVOL, WTVOL,
     *   GRDVOL, DRTVOL, WRKVOL, VCVOL, HATVOL, BPBEM, BPRES, BPWT,
     *   BPGRD, BPDRT, BPWRK, BPHAT, ICENX, ICENY, BOBEM, BORES, BOWT,
     *   BOGRD, BODRT, BOWRK, BOHAT, CNORES, CNOGRD, CNOWRK, CNOWT,
     *   CNOHAT, BUFSZ1, BUFSZ2, BUFSZ3, HATSCR, ICODE
      LOGICAL   FIRST
      CHARACTER BEMFIL*48, RESFIL*48, HATFIL*48, WTFIL*48, GRDFIL*48,
     *   DRTFIL*48, WRKFIL*48, VCFIL*48
      COMMON /VCCOM/ PTRJ, PTRK, GAIN, FLUX, SCRAT,
     *   MAPLIM, GAUSA, GAUSB, GAUSC, GAUSAA, GAUSBB, GAUSCC,
     *   XSPACE, YSPACE, MAPROT, SPEXP,
     *   TVFMAX, APARM, PHAT, RESMAX, RESMIN, RESRMS, HATMAX, HATMIN,
     *   ALPHA,      FIRST,
     *   ICENX, ICENY, NX, NY, NBOXS, WINM, NUMBIN, ICODE, VCLST, VCSTR,
     *   VCLIM, WINB, BPS, LUNBEM, LUNRES, LUNRS1, LUNWT, LUNGD1,
     *   LUNGD2, LUNDRT, LUNWRK, LUNCL1, LUNCL2, LUNHAT, BEMVOL, RESVOL,
     *   WTVOL, GRDVOL, DRTVOL, WRKVOL, VCVOL, HATVOL, CNORES, CNOGRD,
     *   CNOWRK, CNOWT, CNOHAT, BPBEM, BPRES, BPWT, BPGRD, BPDRT, BPWRK,
     *   BPHAT, BOBEM, BORES, BOWT, BOGRD, BODRT, BOWRK, BOHAT, HATSCR
      COMMON /VCCHR/ BEMFIL, RESFIL, HATFIL, WTFIL, GRDFIL, DRTFIL,
     *   WRKFIL, VCFIL
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3,
     *   BUFSZ1, BUFSZ2, BUFSZ3
C                                                          End APVC
LOCAL END
      PROGRAM APVC
C-----------------------------------------------------------------------
C! van Cittert image deconvolution
C# Map AP-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2003, 2006, 2008, 2015
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
C   Use a generalized van Cittert iteration to deconvolve a map. A
C   positivity contraint is possible, but often not wanted.
C     INPUTS    ALIAS   DIM(R)   COMMENTS
C     INNAME    NAME(1,1) 3      Dirty map name
C     INCLASS   CLASS(1,1)2      Dirty map class
C     INSEQ     SEQ(1)    1      Dirty map sequence number
C     INDISK    VOL(1)    1      Dirty map volumn
C     IN2NAME   NAME(1,2) 3      Beam map name
C     IN2CLASS  CLASS(1,2)2      Beam map class
C     IN2SEQ    SEQ(2)    1      Beam map sequence number
C     IN2DISK   VOL(2)    1      Beam map volumn
C     OUTNAME   NAME(1,3) 3      VC map name
C     OUTCLASS  CLASS(1,3)2      VC map class
C     OUTSEQ    SEQ(3)    1      VC map sequence number
C     OUTDISK   VOL(3)    1      VC map volumn
C     DOTV      ITV       1      1 = request TV display of VC
C     NBOXES    NBOXS     1      Number of boxes for VC
C     BOX(4,10) WINM     40      Windows for the VC boxes.
C                                maps and ask user if continue,
C                                < 0 => no display or question
C     GAIN      GAIN      1      VC loop gain
C     NITER     VCLIM     1      Maximum number of VC iterations
C     PHAT      PHAT      1      Offset to beam. Cornwell's smooth
C                                stabilizer.
C     OPCODE    ICODE    12      The algorithm to be used in the
C                                reconstruction. Valid codes are
C                                'WT','ST','VC','TL' (weighted,
C                                Steer, van Cittert (GS), and TUNNL).
C                                Any of these can be prefixed by a
C                                '+' to indicate 'apply positivity'.
C                                Coresponding values are 2,4,3,1 (+/-).
C     APARM     various  10      Various iteration parameters.
C     BADDISK   IBAD     10      Bad disk list.
C-----------------------------------------------------------------------
      INTEGER   TVPASS, IDIR, IERR, IRET, ITER, I, USID, VOL(3), SEQ(3),
     *   ITV, CATBLK(256), CORN(7), JWIN(4), IER, APKEY, NEED
      LOGICAL   FINISH, OLD, EQUAL
      REAL      CATR(128), RMAX, RMIN, SMAX, SMIN
      CHARACTER SCRFIL*48, NAME(3)*12, CLASS(3)*6
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DAPM.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATR(1), CATBLK(1))
      DATA CORN, JWIN /7*1, 4*0/
C-----------------------------------------------------------------------
C                                       Release AP memory
      CALL QRLSE
      NCFILE = 0
      NSCR = 0
C                                       Get input values.
      CALL GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IRET)
C                                       Check for restart of AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Get map files and create
C                                       output files if required.
      CALL FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IRET)
      IF (IRET.NE.0) GO TO 990
C
      ITER = 0
      IF (OLD) THEN
         VCSTR = CATBLK(KINIT)
         IF (VCSTR.GT.0) ITER = VCSTR - 1
         WRITE (MSGTXT,1000) VCSTR
         CALL MSGWRT (4)
         END IF
C                                       Write inputs to history and
C                                       log files.
      OLD = (OLD) .AND. (VCSTR.GT.0)
      CALL VCHIS (NAME, VOL, CLASS, SEQ, OLD)
C
C Save the extrema of HAT. Initialize HAT map if necessary.
C
      IF (.NOT.OLD) THEN
         HATMAX = 1.E-7
         HATMIN = 0.
         CALL FLAT (HATMAX, IRET)
         IF (IRET.NE.0) GO TO 990
      ELSE
         HATMAX = CATR(KRDMX)
         HATMIN = CATR(KRDMN)
         END IF
C
      TVPASS = 2
C                                        Make WEIGHT file.
      IDIR = 1
      NEED = 4 * NY * (NX/2 + 1)
      NEED = NEED / 1024
      CALL QINIT (NEED, 0, APKEY)
      IF ((APKEY.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = (NY * (NX/2 + 1)) / 2
         NEED = NEED / 1024
         NEED = MIN (16 * 1024, NEED) + 4
         CALL QINIT (NEED, 0, APKEY)
         IF ((APKEY.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'CANNOT GET NEEDED AP MEMORY'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 990
            END IF
         END IF
      CALL APDFFT (RMAX, RMIN, IDIR, IRET)
      CALL QRLSE
      IF (IRET.NE.0) GO TO 990
C
C
C                                       Begin VCing.
      FIRST = .TRUE.
      ITER = VCSTR
 100  CONTINUE
         ITER = ITER + 1
C
C  Create RES file. This is the current estimate(HAT)
C  convolved with the beam(WT)
C
         CALL QINIT (NEED, 0, APKEY)
         IF ((APKEY.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'CANNOT GET NEEDED AP MEMORY IN LOOP'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 990
            END IF
         CALL CNVLVE (SMAX, SMIN, IRET)
         CALL QRLSE
         IF (IRET.NE.0) GO TO 990
C
C  Ouptut start of iteration message.
C
         WRITE (MSGTXT,1100) ITER
         CALL MSGWRT (4)
         WRITE (MSGTXT,1300) SMAX, SMIN
         CALL MSGWRT (4)
C
C Subtract RES from DRT to obtain the RES file.
C This updates RESMIN,RESMAX,RESRMS.
C
         CALL SUBMAP ( IRET )
         IF (IRET.NE.0) GO TO 990
C
C Using the HAT,RES files calculate the new map
C and store it in the HAT file. This updates HATMIN,HATMAX.
C
         CALL VCING( IRET )
         IF (IRET.NE.0) GO TO 990
C
C Summarize end of iteration.
C
         WRITE (MSGTXT,1210) RESMAX,RESMIN
         CALL MSGWRT (4)
         WRITE (MSGTXT,1400) HATMAX,HATMIN
         CALL MSGWRT (4)
C
C Display current HAT map.
C
         IF (ITV.GT.0) THEN
            CALL QINIT (NEED, 0, APKEY)
            IF ((APKEY.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               MSGTXT = 'CANNOT GET NEEDED AP MEMORY FOR TV'
               CALL MSGWRT (8)
               IRET = 8
               GO TO 990
               END IF
            IF (ITV.EQ.1) CALL DISPTV (TVPASS)
            CALL QRLSE
            END IF
C
C  Make it no longer the first iteration.
C
         FIRST = .FALSE.
C
C  Check for end.
C
         FINISH = ITER.GE.VCLIM .OR. TVPASS.EQ.32700
C
C If more VCing required, then loop.
C
         IF (.NOT.FINISH) GO TO 100
C
C  Make output header.
C
      CATR(KRDMX) = HATMAX
      CATR(KRDMN) = HATMIN
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KITYP) = 4
      CATBLK(KINIT) = ITER
      CALL CATIO ('UPDT', VOL(3), CCNO, CATBLK, 'REST', BUFF3, IERR)
      IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (4)
         END IF
C                                       Find HAT file
      HATSCR = 0
      DO 300 I = 1,NSCR
         CALL ZPHFIL ('SC', SCRVOL(I), SCRCNO(I), 1, SCRFIL, IER)
         EQUAL = SCRFIL.EQ.HATFIL
         IF (EQUAL.AND.(SCRVOL(I).EQ.HATVOL)) HATSCR = I
         IF (HATSCR.GT.0) GO TO 310
 300     CONTINUE
C                                       Lost output file
         IRET = 8
         WRITE (MSGTXT,1301)
         CALL MSGWRT (8)
         GO TO 990
C                                       Copy to output
 310  CALL PLNPUT (VOL(3), CCNO, CORN, JWIN, HATSCR, NX, NY, BUFF1,
     *   BUFF2, BUFSZ1, BUFSZ2, LUNDRT, LUNHAT, IERR)
C                                        Finished.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('RESTARTING VCING AFTER ',I5,' ITERATIONS')
 1100 FORMAT ('ITERATION = ',I5)
 1200 FORMAT ('ERROR',I3,' UPDATING VC HEADER ')
 1210 FORMAT ('  RESIDUAL MAP  : MAX.,MIN.    = ',1PE12.4,
     *     ',',1PE12.4)
 1300 FORMAT ('  CONVOLVED MAP : MAX.,MIN.    = ',1PE12.4,',',
     *   1PE12.4)
 1301 FORMAT ('I SEEM TO HAVE LOST THE OUTPUT FILE')
 1400 FORMAT ('  MAP ESTIMATE  : MAX.,MIN.    = ',1PE12.4,
     *     ',',1PE12.4)
      END
      SUBROUTINE SUBMAP (IERR)
C-----------------------------------------------------------------------
C   SUBMAP subtracts the RES file ( = beam * map ) from the Dirty map
C   producing the residual map.
C   The WORK file is used as the output file and then
C   the physical files for the Residual and WORK files are
C   exchanged when the routine is finished.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      INTEGER   IERR
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, WIN(4),
     *   I, J, IBOX, IO, IG, ID
      LOGICAL   MAP, EXCL, WAIT
      REAL   RNXY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA MAP, EXCL, WAIT /.TRUE., 2*.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize extrema.
      RESMAX = -1.0E20
      RESMIN =  1.0E20
      RESRMS =  0
      RNXY   =  0
C                                       Open and INIT files.
      CALL ZOPEN (LUNDRT, FIND1, DRTVOL, DRTFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      CALL ZOPEN (LUNRES, FIND2, RESVOL, RESFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      CALL ZOPEN (LUNWRK, FIND3, WRKVOL, WRKFIL, MAP, EXCL, WAIT, IERR)
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
      CALL MINIT ('READ', LUNRES, FIND2, NX, NY, WIN, BUFF2, BUFSZ2,
     *   BORES, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      CALL MINIT ('WRIT', LUNWRK, FIND3, NX, NY, WIN, BUFF3, BUFSZ3,
     *   BOWRK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Loop thru map.
      DO 150 I = 1,NY
C                                       Read dirty map.
         CALL MDISK ('READ', LUNDRT, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR, I
            GO TO 990
            END IF
C                                       Write output.
         CALL MDISK ('WRIT', LUNWRK, FIND3, BUFF3, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR,I
            GO TO 990
            END IF
C                                       Read GRID map
         CALL MDISK ('READ', LUNRES, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1105) IERR,I
            GO TO 990
            END IF
         CALL RFILL (NX, 0.0, BUFF3(BIND3))
         DO 140 J = 1,NX
            DO 135 IBOX = 1,NBOXS
               IF ((J.GE.WINM(1,IBOX)) .AND. (J.LE.WINM(3,IBOX)) .AND.
     *            (I.GE.WINM(2,IBOX)) .AND. (I.LE.WINM(4,IBOX))) THEN
                  IO = BIND3+J-1
                  IG = BIND2+J-1
                  ID = BIND1+J-1
                  BUFF3(IO) = BUFF1(ID) - BUFF2(IG)
                  RESMAX = MAX(BUFF3(IO), RESMAX)
                  RESMIN = MIN(BUFF3(IO), RESMIN)
                  RESRMS = RESRMS + BUFF3(IO)**2
                  RNXY = RNXY + 1
                  GO TO 140
                  END IF
 135           CONTINUE
 140        CONTINUE
 150     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUNWRK, FIND3, BUFF3, BIND3, IERR)
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
      CALL ZCLOSE (LUNRES, FIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1170) IERR
         CALL MSGWRT (8)
         END IF
      CALL ZCLOSE (LUNWRK, FIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1180) IERR
         CALL MSGWRT (8)
         END IF
C
C                                       Return RMS value.
      RESRMS = SQRT(RESRMS/RNXY)
C                                       Switch RES and WRK files.
      CALL FSWTCH (RESFIL, WRKFIL, RESVOL, WRKVOL, CNORES, CNOWRK,
     *   BORES, BOWRK)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUBMAP: CANNOT OPEN DIRTY FILE, ERROR ',I3)
 1020 FORMAT ('SUBMAP: CANNOT OPEN FOR READ RESIDUAL MAP, ERROR ',I3)
 1030 FORMAT ('SUBMAP: CANNOT OPEN FOR WRITE RESIDUAL MAP, ERROR ',I3)
 1040 FORMAT ('SUBMAP: CANNOT INIT DIRTY MAP, ERROR ',I3)
 1050 FORMAT ('SUBMAP: CANNOT INIT READ RESIDUAL MAP, ERROR ',I3)
 1060 FORMAT ('SUBMAP: CANNOT INIT WRITE RESIDUAL MAP, ERROR ',I3)
 1070 FORMAT ('SUBMAP: READ ERROR ',I3,' DIRTY FILE ROW ',I6)
 1105 FORMAT ('SUBMAP: READ ERROR ',I3,' RESIDUAL MAP ROW ',I6)
 1100 FORMAT ('SUBMAP: WRITE ERROR ',I3,' RESIDUAL ROW ',I6)
 1150 FORMAT ('SUBMAP: FINISH ERROR ',I3,' RESIDUAL FILE')
 1160 FORMAT ('SUBMAP: CLOSE ERROR ',I3,' DIRTY MAP ')
 1170 FORMAT ('SUBMAP: CLOSE ERROR ',I3,' READ RESIDUAL FILE')
 1180 FORMAT ('SUBMAP: CLOSE ERROR ',I3,' WRITE RESIDUAL FILE ')
      END
      SUBROUTINE APDFFT (SMAX, SMIN, IDIR, IERR)
C-----------------------------------------------------------------------
C   APDFFT calls AP FFT routines PASS1 and PASS2.  If the transform
C   can be done all in the AP this is done; if not then a transpose
C   of the type designed by F. Schwab is done.
C   The work file is used in
C   the transpose between passes of the transform.
C   The beam in R   form is assumed in file GRD.
C   INPUT:
C     IDIR       I    Type of transform :
C                          IDIR = -1 : GRID => RES
C                          IDIR = 1  : BEAM => WT
C                          IDIR = 2  : MAP => GRID
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
C     BOHAT      I    Block offset for VC map.
C     HATVOL     I    Volume for VC map.
C     HATFIL     C    Physical name of VC map.
C     LUNHAT     I    LUN for VC map.
C     BOWRK      I    Block offset for WORK file.
C     WRKVOL     I    Volume for work file.
C     WRKFIL     C    Physical name of WORK file.
C     LUNWRK     I    LUN for WORK file.
C     BUFF1(),BUFF2()  I    Work buffers for I/O.
C     BUFSZ1,BUFSZ2    I    Size in bytes of BUFF1 and BUFF2.
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      INTEGER   IERR
      INTEGER   IDIR, LUN(3), BO(3), VOL(3), IDIRT
      LOGICAL   F
      REAL      SMAX, SMIN
      CHARACTER FIL(3)*48
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       REAL TO REAL
C                                       Set file information.
C                                       1 = Beam, 2 = Work, 3 = Weight
      IF ((IDIR.GE.0) .AND. (IDIR.NE.2)) THEN
         IDIRT = IDIR
         BO(1) = BOGRD
         BO(2)= BOWRK
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
         CALL PASS1 (IDIRT, F, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (IDIRT, F, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, SMAX, SMIN, IERR)
C                                       REAL TO COMPLEX
C                                       Set file information.
C                                       1 = Map , 2 = Work, 3 = Grid
      ELSE IF (IDIR.EQ.2) THEN
         IDIRT = 3
         BO(1) = BOHAT
         BO(2) = BOWRK
         BO(3) = BOGRD
         VOL(1) = HATVOL
         VOL(2) = WRKVOL
         VOL(3) = GRDVOL
         LUN(1) = LUNHAT
         LUN(2) = LUNWRK
         LUN(3) = LUNGD1
         FIL(1) = HATFIL
         FIL(2) = WRKFIL
         FIL(3) = GRDFIL
C                                       Do disk based FFT.
         CALL PASS1 (IDIRT, F, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (IDIRT, F, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, SMAX, SMIN, IERR)
C                                       COMPLEX TO REAL
C                                       Set file information.
C                                       1 = Grid, 2 = Work, 3 = Resid.
      ELSE
         BO(1) = BOGRD
         BO(2) = BOWRK
         BO(3) = BORES
         VOL(1) = GRDVOL
         VOL(2) = WRKVOL
         VOL(3) = RESVOL
         LUN(1) = LUNGD1
         LUN(2) = LUNWRK
         LUN(3) = LUNRES
         FIL(1) = GRDFIL
         FIL(2) = WRKFIL
         FIL(3) = RESFIL
C                                       Do disk based FFT.
         CALL PASS1 (IDIR, F, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (IDIR, F, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NX, NY, SMAX, SMIN, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE VCHIS (NAME, VOL, CLASS, SEQ, OLD)
C-----------------------------------------------------------------------
C   VCHIS copies the dirty map history if the VC map does not
C   already have a history file.  Then the inputs to VC with
C   the default values are added to the history file.
C   INPUTS:
C     NAME(6,3)   C*12 Names of maps
C                      1 = Dirty map
C                      2 = Beam map
C                      3 = VC map
C     VOL(3)      I    Vol. numbers of the maps.
C     CLASS(3)    I    Classes of the map.
C     SEQ(3)      I    Sequence numbers of the maps.
C     OLD         L    T => VC map pre-exists & restart > 0
C   From common /MAPHDR/
C     CATBLK(256) I    Catalog header block for the VC map.
C   From COMMON /VC/
C     NX          I    Number of cell in the map in RA.
C     NY          I    Number of cells in the map in dec.
C     VCLIM       I    Max number of iterations.(NITER)
C     GAIN        R    VC loop gain.
C     BUFF1()     I    Work buffer
C     BUFF2()     I    Work buffer
C     LUNCL1      I    LUN for VC history file
C     LUNDRT      I    LUN for dirty history file
C   From COMMON /CFILES/
C     FCNO(10)    I    Catalog slot numbers of catalogd maps.
C                      2 = Dirty map
C                      3 = VC map
C   OUTPUT:
C     The input parameters are written on the history file and the
C     log file.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      CHARACTER NAME(3)*12, CLASS(3)*6, XCLS*8, XNAM*12, OPCODS(11)*4
      INTEGER   VOL(3), SEQ(3), CATBLK(256), IERR, I, J
      LOGICAL   T, OLD
      CHARACTER HILINE*72
      INTEGER   ITEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /MAPHDR/ CATBLK
      DATA T /.TRUE./
      DATA OPCODS/'PC  ','ST  ','VC  ','WT  ','TL  ','    ',
     *             '+TL ','+WT ','+VC ','+ST ','+PC '/
C-----------------------------------------------------------------------
C                                       Open old file: no copy old HI
      IF (OLD) THEN
         CALL HIOPEN (LUNCL1, VOL(3), CCNO, BUFF2, IERR)
         IF (IERR.EQ.5) GO TO 20
         IF (IERR.EQ.0) THEN
            CALL ZDATE (BUFF1(1))
            CALL ZTIME (BUFF1(4))
            CALL TIMDAT (BUFF1(4), BUFF1(1), XCLS, XNAM)
            WRITE (HILINE,1000) TSKNAM, RLSNAM, XNAM, XCLS
            CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 50
            GO TO 30
         ELSE
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 60
            END IF
         END IF
C                                       Copy/open history files.
 20   CALL KEYPCP (VOL(1), FCNO(2), VOL(3), CCNO, 0, ' ', IERR)
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
 30   WRITE (MSGTXT,2000) TSKNAM, NAME(1), CLASS(2), SEQ(1), VOL(1)
      CALL MSGWRT (3)
      CALL HENCO1 (TSKNAM, NAME(1), CLASS(1), SEQ(1), VOL(1),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Dirty beam name.
      WRITE (MSGTXT,2001) TSKNAM,NAME(2),CLASS(2),SEQ(2),VOL(2)
      CALL MSGWRT (3)
      CALL HENCO2 (TSKNAM, NAME(2), CLASS(2), SEQ(2), VOL(2),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       VC map name.
      WRITE (MSGTXT,2002) TSKNAM,NAME(3),CLASS(3),SEQ(3),VOL(3)
      CALL MSGWRT (3)
      CALL HENCOO (TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Maximum number of Iterations.
      WRITE (HILINE,2003) TSKNAM, VCLIM
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Loop gain.
      WRITE (HILINE,2005) TSKNAM, GAIN
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Smooth stablizer.
      WRITE (HILINE,2006) TSKNAM, PHAT
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Opcode parameter.
      ITEMP = 6 + ICODE
      WRITE (HILINE,2007) TSKNAM, OPCODS(ITEMP)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Parameters.
      WRITE (HILINE,2008) TSKNAM, (APARM(I), I = 1,4)
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Number of boxes.
      WRITE (HILINE,2009) TSKNAM, NBOXS
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Windows.
      DO 40 I = 1,NBOXS
         WRITE (HILINE,2010) TSKNAM, I, (WINM(J,I), J = 1,4)
         CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
 40      CONTINUE
      GO TO 60
C                                       Error has occured.
 50   WRITE (MSGTXT,1050) IERR
      CALL MSGWRT (6)
C                                       Close history files.
 60   CALL HICLOS (LUNCL1, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* RESTART ',
     *   A12,2X,A8)
 1010 FORMAT ('UNABLE TO OPEN OLD VC-MAP HISTORY FILE')
 1020 FORMAT ('VCHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT ('VCHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT (A6,' DIRTY MAP = ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2001 FORMAT (A6,' BEAM  MAP = ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2002 FORMAT (A6,' VC    MAP = ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2003 FORMAT (A6,' NITER = ',I6,' /MAX. NO. VC ITERATIONS')
 2005 FORMAT (A6,' GAIN = ',F7.4,' /VC LOOP GAIN FACTOR')
 2006 FORMAT (A6,' PHAT = ',F7.4,' /PRUSSIAN HAT PARAM')
 2007 FORMAT (A6,' OPCODE = ',A4, '/OPCODE PARAMETER')
 2008 FORMAT (A6,' APARM  = ',4F10.3)
 2009 FORMAT (A6,' NBOXES = ',I3,' /NO. VC WINDOWS')
 2010 FORMAT (A6,' BOX(',I2,') = ',I5,', ',I5,', ',I5,', ',I5,
     *   ' /VC WINDOW')
      END
      SUBROUTINE FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   FILES reads and floats the  beam and creates the
C   VC map and components files. The R   beam is placed in the
C   GRD file and the location of the R   catalogd files are
C   left in the DRT and BEM files.
C   BEAM and Dirty maps in catalog are marked READ and VC map
C   is marked WRITE.
C   INPUT:
C     USID     I    User number.
C     NAME(3)  C    Names of maps
C                     1 = Dirty map
C                     2 = Beam map
C                     3 = VC map
C     CLASS(3) C    Classes of the maps
C     VOL(3)     I    Volumns of the maps.
C   OUTPUT:
C     ICENX    I    Offset in X of BEAM center from NX/2+1
C     ICENY    I    Offset in Y of BEAM center from NY/2+1
C     OLD      L    TRUE if old map file being replaced.
C   Commons:
C     CATBLK(256) in /MAPHDR/ a preliminary header for the VC map
C        is returned.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      CHARACTER  NAME(3)*12, CLASS(3)*6, STAT*4, MAPUNT*8, MTYPE*2
      INTEGER   USID, CATBLK(256), CNO,
     *   VOL(3), SEQ(3), OLDIM, OLDNAX(7), IDEPTH(5),
     *   IERR, IBOX, IER, I, IMCENX, IMCENY, NREC, LREC
      INTEGER   CORN(7), JWIN(4), NOSCR, IROUND
      LOGICAL   OLD
      DOUBLE PRECISION CATD(128), OLDCRV(7)
      REAL      CATR(128), XCEN, YCEN
      INTEGER   ISIZE
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA CORN, JWIN, IDEPTH /7*1, 4*0, 5*1/
      DATA MAPUNT /'JY/PIXEL'/
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
C                                       Mark file 'READ' in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(2)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 0
C                                       Determine NX and NY
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Determine beam center pixel
      CALL PEAKFN (LUNBEM, VOL(2), CNO, IDEPTH, CATBLK, BUFF3,
     *   BUFF3, BUFSZ3, XCEN, YCEN, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGTXT,1777) XCEN,YCEN
      CALL MSGWRT (4)
      XCEN = XCEN - NX/2 - 1
      YCEN = YCEN - NY/2 - 1
      ICENX = IROUND (XCEN)
      ICENY = IROUND (YCEN)
C                                       Create scratch files.
C                                       Residual file
      RESVOL = MAX (1, VOL(2))
      NSCR = 0
      ISIZE = MAX (NX, 64) + 3
      ISIZE = (ISIZE * NY) * 4.0D0 + 2048.0D0
      ISIZE = (ISIZE - 1) / 512 + 1
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      RESVOL = SCRVOL(NSCR)
      CNORES = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, RESFIL, IERR)
C                                       Grid file
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      GRDVOL = SCRVOL(NSCR)
      CNOGRD = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, GRDFIL, IERR)
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
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, WRKFIL, IERR)
C                                       Weight file
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      WTVOL = SCRVOL(NSCR)
      CNOWT = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, WTFIL, IERR)
C                                       Hat file
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1037) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      HATVOL = SCRVOL(NSCR)
      CNOHAT = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, HATFIL, IERR)
      HATSCR = NSCR
C
C                                       Beam map into scratch file
      CALL PLNGET (VOL(2), CNO, CORN, JWIN, ICENX, ICENY, NOSCR, NX,
     *   NY, BUFF1, BUFF2, BUFSZ1, BUFSZ2, LUNGD1, LUNDRT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
C                                       Get and store info on BEAM.
C                                       Loop through the boxes.
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
 70      CONTINUE
C                                       Store information on BEAM file
C                                       Use GRD file for temp R   BEAM.
      CALL ZPHFIL ('MA', VOL(2), CNO, 1, BEMFIL, IER)
      BEMVOL = VOL(2)
      SEQ(2) = CATBLK(KIIMS)
      BOBEM = 1
C                                       Get catalog slot number for
C                                       the dirty map and it's CATBLK.
      CNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', VOL(1), CNO, NAME(1), CLASS(1), SEQ(1),
     *    MTYPE, USID, STAT, BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(1), CLASS(1), SEQ(1), VOL(1),
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
C                                       Warn if "VC"
      IF (CATBLK(KITYP).EQ.4) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (4)
         END IF
C                                       Get dirty scale,offset in COMMON
      CALL ZPHFIL ('MA', VOL(1), CNO, 1, DRTFIL, IER)
      DRTVOL = VOL(1)
      BODRT = 1
C                                        RA spacing.
      XSPACE = CATR(KRCIC)
C                                        Dec spacing.
      YSPACE = CATR(KRCIC+1)
C                                        Map rotation.
      MAPROT = CATR(KRCRT+1)
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
      CALL CHR2H (12, NAME(3), KHIMNO, CATR(KHIMN))
      CALL CHR2H (6, CLASS(3), KHIMCO, CATR(KHIMC))
      CATBLK(KIIMS) = SEQ(3)
C                                        Put in units of map
      CALL CHR2H (8, MAPUNT, 1, CATR(KHBUN))
C                                       Create output VC map file.
      CALL MCREAT (VOL(3), CNO, BUFF1, IERR)
      OLD = .FALSE.
      IF (IERR.NE.0) THEN
         OLD = .TRUE.
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1190) IERR
            GO TO 990
C                                       Existing file: check it out
         ELSE
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
               IF (OLDCRV(I).NE.CATD(KDCRV+I-1)) GO TO 215
 210           CONTINUE
C                                       Fetch
            IMCENX = 0
            IMCENY = 0
            CALL PLNGET (VOL(3), CNO, CORN, JWIN, IMCENX, IMCENY,
     *         HATSCR, NX, NY, BUFF1, BUFF2, BUFSZ1, BUFSZ2, LUNGD1,
     *         LUNDRT, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1038) IERR
               GO TO 990
               END IF
C                                       It matches, mark WRITE
            CALL CATIO ('WRIT', VOL(3), CNO, CATBLK, 'WRIT', BUFF3,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1210) IERR
               CALL MSGWRT (8)
               END IF
            GO TO 300
            END IF
C                                       Not same file !!!
 215     IERR = 8
         WRITE (MSGTXT,1215)
         GO TO 990
         END IF
C                                       Actual seq #
 300  SEQ(3) = CATBLK(KIIMS)
C                                       Add to /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(3)
      VCVOL = VOL(3)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
      IF (OLD) FRW(NCFILE) = 1
      CCNO = CNO
      NREC = VCLIM + 10
      LREC = 3
C                                      Close.
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILES: ERR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK',I2,' USID',I5)
 1010 FORMAT ('FILES: CANNOT COPY BEAM CATBLK, ERROR',I3)
 1015 FORMAT ('FILES: ERROR',I3,' CREATING RESIDUAL SCRATCH FILE')
 1020 FORMAT ('FILES: ERROR',I3,' CREATING GRID SCRATCH FILE')
 1025 FORMAT ('FILES: ERROR',I3,' CREATING WORK SCRATCH FILE')
 1030 FORMAT ('FILES: ERROR',I3,' CREATING WEIGHT SCRATCH FILE')
 1037 FORMAT ('FILES: ERROR',I3,' CREATING HAT SCRATCH FILE')
 1038 FORMAT ('FILES: ERROR',I3,' READING IMAGE')
 1035 FORMAT ('FILES: CANNOT COPY DIRTY BEAM, ERROR ',I3)
 1090 FORMAT ('FILES: CANNOT COPY MAP CATBLK, ERROR',I3)
 1100 FORMAT ('WARNING: MAY BE VCING A VC MAP')
 1105 FORMAT ('FILES: UNEQUAL DIMENSIONS IN DIRTY AND BEAM MAPS')
 1106 FORMAT ('       BEAM =',2I5,' DIRTY =',2I5)
 1190 FORMAT ('FILES: COULD NOT CREATE VC MAP FILE, ERROR ',I3)
 1210 FORMAT ('FILES: CANNOT UPDATE VC CATBLK, ERROR',I3)
 1215 FORMAT ('OLD VC MAP NOT COMPATIBLE WITH DIRTY MAP')
 1777 FORMAT ('    PEAK OF BEAM IS AT ',2(F6.0,1X))
      END
      SUBROUTINE GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IERR)
C-----------------------------------------------------------------------
C   GETIN gets the input parameters for the program from AIPS
C  See Prologue for APVC for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, NAME(3)*12, CLASS(3)*6,
     *   DEFCLS*6, CODES(5)*4, XOPCOD*4, MTYPE*2
      INTEGER   USID, VOL(3), SEQ(3), ITV, IERR, I4TEMP
      INTEGER   NPARMS, CATBLK(256), IND, IROUND
      INTEGER   ITEMP, NPT, CNO, I, IL, IK, J
      HOLLERITH XNAM1(3), XNAM2(3), XNAM3(3), XCLAS1(2), XCLAS2(2),
     *   XCLAS3(2), XXOPCO
      REAL   DEFAUL(2,5)
      REAL   XSEQ1, XSEQ2, XSEQ3, XVOL1, XVOL2, XVOL3, XG,
     *   XNI, XDOTV, XNB, XBOX(4,10), XBAD(10), XPHAT, XPARM(10)
      LOGICAL   T, F, EQUAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XVOL1, XNAM2, XCLAS2,
     *   XSEQ2, XVOL2, XNAM3, XCLAS3, XSEQ3, XVOL3,
     *   XDOTV, XNB, XBOX, XG, XNI, XPHAT, XXOPCO, XPARM, XBAD
      DATA PRGNAM /'APVC  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA CODES   /'TL',  'WT',   'VC',  'ST',   'PC'/
      DATA DEFAUL /0.,0., 0.5,0., 0.,0., 0.8,0., 0.5,4./
C-----------------------------------------------------------------------
C                                        Initialize common parameters.
C                                        global areas
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      CALL HIINIT (3)
C                                        non-standard messages
C                                        VC common areas
      BOBEM = 1
      BOHAT = 1
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
      LUNHAT = 24
      LUNCL1 = 26
C
      NUMBIN = 1024
      I4TEMP = 2  * MABFSS
      BUFSZ1 = I4TEMP
      BUFSZ2 = I4TEMP
      BUFSZ3 = I4TEMP
      BPS = NBPS
      TVFMAX = 0.0
C                                       Get AIPS adverbs. NPARM=# reals
      NPARMS = 87
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
C
      CALL YTVCIN
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAM1, NAME(1))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (12, 1, XNAM2, NAME(2))
      CALL H2CHR (6, 1, XCLAS2, CLASS(2))
      CALL H2CHR (12, 1, XNAM3, NAME(3))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
      CALL H2CHR (4, 1, XXOPCO, XOPCOD)
C                                       get actual dirty map name
      VOL(1) = IROUND (XVOL1)
      SEQ(1) = IROUND (XSEQ1)
      USID = NLUSER
      MTYPE = 'MA'
      CALL MAPOPN ('READ', VOL, NAME(1), CLASS(1), SEQ, MTYPE, USID,
     *   LUNBEM, IND, CNO, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL MAPCLS ('READ', VOL, CNO, LUNBEM, IND, CATBLK, F, BUFF1,
     *   IERR)
C                                       Default name for Beam and VC
C                                       maps is Dirty map name.
      EQUAL = NAME(2) .EQ. '            '
      IF (EQUAL) NAME(2) = NAME(1)
C                                       Copy CLASSes or default.
      EQUAL = CLASS(2) .EQ. '      '
      IF (EQUAL) THEN
         CLASS(2) = 'IBEM  '
         IF (CLASS(1).EQ.'RMAP  ') CLASS(2) = 'RBEM  '
         IF (CLASS(1).EQ.'LMAP  ') CLASS(2) = 'LBEM  '
         END IF
      DEFCLS = 'IVC   '
      IF (CLASS(1).EQ.'RMAP  ') DEFCLS = 'RVC   '
      IF (CLASS(1).EQ.'LMAP  ') DEFCLS = 'LVC   '
      IF (CLASS(1).EQ.'QMAP  ') DEFCLS = 'QVC   '
      IF (CLASS(1).EQ.'UMAP  ') DEFCLS = 'UVC   '
      IF (CLASS(1).EQ.'VMAP  ') DEFCLS = 'VVC   '
C                                       Get volumn numbers
      VOL(2) = IROUND (XVOL2)
      VOL(3) = IROUND (XVOL3)
C                                       Get sequence numbers
      SEQ(2) = IROUND (XSEQ2)
      SEQ(3) = IROUND (XSEQ3)
      IF (SEQ(2).LE.0) SEQ(2) = SEQ(1)
      CALL MAKOUT (NAME, CLASS, SEQ, DEFCLS, NAME(3), CLASS(3),
     *   SEQ(3))
C                                       Get VCing information
      GAIN = 0.5
      VCLIM = 20
      PHAT = 0.
      IF (XG.GT.0.0)    GAIN = XG
      IF (XNI.GT.0.5)   VCLIM = IROUND (XNI)
      IF (XPHAT.GT.0.0) PHAT = XPHAT
C
C  Save parameters.
C
      DO 42 I=1,10
         APARM(I) = XPARM(I)
 42      CONTINUE
C
C                                       Determine which operation to
C                                       perform.
      ICODE = 0
      ITEMP = -1
      NPT = 1
      EQUAL = XOPCOD(1:1) .EQ. '+'
      IF (EQUAL) THEN
         NPT = 2
         ITEMP = 1
         END IF
      ICODE = ITEMP * 3
      DO 46 I=1,5
         EQUAL = CODES(I)(1:2) .EQ. XOPCOD(NPT:NPT+1)
         IF (EQUAL) THEN
            ICODE = ITEMP * I
            IF(APARM(2).LE.0) APARM(2) = DEFAUL(1,I)
            IF(APARM(3).LE.0) APARM(3) = DEFAUL(2,I)
            END IF
 46      CONTINUE
C                                       Check if TV allowed
      ITV = 0
      IF (XDOTV.GT.0.0 .AND. NTVDEV.GT.0.0) ITV = 1
C
C                                       Boxes information.
      NBOXS = IROUND (XNB)
      NBOXS = MIN (NBOXS, 10)
      NBOXS = MAX (NBOXS, 1)
      IL = 1
      DO 60 I = 1,10
         IK = 0
         DO 50 J = 1,4
            WINM(J,IL) = IROUND (XBOX(J,I))
            IF (WINM(J,IL).GT.0) IK = IK + 1
 50         CONTINUE
         IF ((I.EQ.1) .OR. (IK.GT.0)) IL = IL + 1
 60      CONTINUE
      NBOXS = MIN (IL-1, NBOXS)
C
C                                       Get bad disks.
C
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 70      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 4000 FORMAT ('YOU ARE USING A NON-STANDARD, EXPERIMENTAL PROGRAM')
      END
      SUBROUTINE CNVLVE (SMAX, SMIN, IERR)
C-----------------------------------------------------------------------
C   CNVLVE Fourier convolves the current map estimate with the beam.
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, IERR, IDIR, JLIM, I, J, IO, IW, IG,
     *   WIN(4), BIND1, BIND2, BIND3
      LOGICAL   MAP, EXCL, WAIT
      REAL      WBUFF(1), GBUFF(1), OBUFF(1), SMAX,
     *   SMIN, RNXY, GAMMA, RMS, WT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (WBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      EQUIVALENCE (OBUFF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,2*.TRUE./
C-----------------------------------------------------------------------
C                                        Do FFT to get transform of map
      IDIR = 2
      CALL APDFFT (SMAX, SMIN, IDIR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Open weights file and init.
      CALL ZOPEN (LUNWT, FIND1, WTVOL, WTFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NY
      WIN(4) = NX / 2 + 1
      CALL MINIT ('READ', LUNWT, FIND1, WIN(3), WIN(4), WIN, WBUFF,
     *   BUFSZ1, BOWT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Open and INIT GRID file for read
      CALL ZOPEN (LUNGD1, FIND2, GRDVOL, GRDFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      WIN(3) = NY*2
      WIN(4) = NX / 2 + 1
      CALL MINIT ('READ', LUNGD1, FIND2, WIN(3), WIN(4), WIN, GBUFF,
     *   BUFSZ2, BOGRD, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Open and INIT WRK file for writi
      CALL ZOPEN (LUNWRK, FIND3, WRKVOL, WRKFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      CALL MINIT ('WRIT', LUNWRK, FIND3, WIN(3), WIN(4), WIN, OBUFF,
     *   BUFSZ3, BOWRK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Begin loop thru map.
      JLIM = NX / 2 + 1
      RNXY = REAL(NX)*REAL(NY)
      GAMMA = 0.
      RMS   = 0.
      DO 150 I = 1,JLIM
C                                       Read weights.
         CALL MDISK ('READ', LUNWT, FIND1, WBUFF, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1080) IERR,I
            GO TO 990
            END IF
C                                       Read row from GRID file.
         CALL MDISK ('READ', LUNGD1, FIND2, GBUFF, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR,I
            GO TO 990
            END IF
C                                       Write file back to disk.
         CALL MDISK ('WRIT', LUNWRK, FIND3, OBUFF, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR, I
            GO TO 990
            END IF
C                                        Do each row
         DO 140 J=1,NY
C                                       Multiply by weights.
            IO = BIND3+2*(J-1)
            IW = BIND1+  (J-1)
            IG = BIND2+2*(J-1)
            WT = RNXY*WBUFF(IW) + PHAT
            GAMMA = MAX(GAMMA,WT)
            RMS = RMS + WT**2
            OBUFF(IO) = WT*GBUFF(IG)
            OBUFF(IO+1) = WT*GBUFF(IG+1)
 140        CONTINUE
 150     CONTINUE
C                                       Set GAMMA.
      IF (FIRST) THEN
         RMS = SQRT( 2.*RMS/RNXY)
         WRITE (MSGTXT,1155)RMS,GAMMA
         CALL MSGWRT (8)
         GAMMA = SQRT(GAMMA*RMS)
         WRITE (MSGTXT,1158)GAMMA
         CALL MSGWRT (8)
         IF(APARM(1).LE.0)APARM(1) = GAMMA
         END IF
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
C                                       Switch WRK and GRID files.
      CALL FSWTCH (GRDFIL, WRKFIL, GRDVOL, WRKVOL, CNOGRD, CNOWRK,
     *   BOGRD, BOWRK)
C                                        FFT back to map plane
      IDIR = -1
      CALL APDFFT (SMAX, SMIN, IDIR, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1180) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CNVLVE: ERROR ',I3,' IN APDFFT MAP TO U,V')
 1010 FORMAT ('CNVLVE: ERROR ',I3,' OPENING FILE ')
 1020 FORMAT ('CNVLVE: ERROR ',I3,' INIT WEIGHT FILE')
 1030 FORMAT ('CNVLVE: ERROR ',I3,' OPENING GRID FILE ')
 1050 FORMAT ('CNVLVE: ERROR ',I3,' INIT GRID FILE')
 1090 FORMAT ('CNVLVE: WRITE ERROR ',I3,' GRID ROW ',I6)
 1080 FORMAT ('CNVLVE: ERROR ',I3,' READ WEIGHT ROW ',I6)
 1100 FORMAT ('CNVLVE: ERROR ',I3,' READ GRID ROW ',I6)
 1150 FORMAT ('CNVLVE: ERROR ',I3,' CLOSING WEIGHT FILE')
 1155 FORMAT ('GRD FILE RMS =',F9.3,'   MAX =',F9.3)
 1158 FORMAT ('DEFAULT NPPBEAM = ',F9.3)
 1160 FORMAT ('CNVLVE: ERROR ',I3,' CLOSING GRID FILE')
 1170 FORMAT ('CNVLVE: FINISH ERROR ',I3,' WRITING GRID FILE ')
 1180 FORMAT ('CNVLVE: ERROR ',I3,' IN APDFFT U,V TO MAP')
      END
      SUBROUTINE VCING ( IERR )
C-----------------------------------------------------------------------
C     VCING finds the next estimate of the VC map from the current
C     estimate, stored in HAT, and the residuals of the current
C     estimate, stored in RES. The new estimate is written into the
C     HAT file.
C     Programmer : T.J. Cornwell May 1982
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, IERR, I, J, IBOX, IW, IG, IO,
     *   WIN(4), BIND1, BIND2, BIND3, ITEMP
      LOGICAL   MAP, EXCL, WAIT
      REAL      HATBUF(1), RESBUF(1), OBUFF(1),
     *   GAINS, SIGMA2, HMAX, RMAX, TRIM, BETA, RNXY, ERRRMS,
     *   WEIGHT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (RESBUF(1), BUFF1(1)),      (HATBUF(1), BUFF2(1))
      EQUIVALENCE (OBUFF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,2*.TRUE./
C-----------------------------------------------------------------------
C                                          Dividingby GAMMA converts
C                                          from Jy/beam to Jy/pixel.
      GAINS = GAIN/APARM(1)
      SIGMA2 = (RESRMS/APARM(1))**2
      HMAX = MAX(ABS(HATMAX),ABS(HATMIN))
      RMAX = MAX(ABS(RESMAX),ABS(RESMIN))
      TRIM = APARM(2)*RMAX
      ALPHA = APARM(2)
      BETA = 1. - APARM(2)
C
      RNXY = 0.0
      ERRRMS = 0.0
      HATMAX = -1.E20
      HATMIN =  1.E20
C
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                       Open residuals file and init.
      CALL ZOPEN (LUNRES, FIND1, RESVOL, RESFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      CALL MINIT ('READ', LUNRES, FIND1, NX, NY, WIN, RESBUF, BUFSZ1,
     *   BORES, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Open and INIT HAT file for read
      CALL ZOPEN (LUNHAT, FIND2, HATVOL, HATFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      CALL MINIT ('READ', LUNHAT, FIND2, NX, NY, WIN, HATBUF, BUFSZ2,
     *   BOHAT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Open and INIT WRK file for writi
      CALL ZOPEN (LUNWRK, FIND3, WRKVOL, WRKFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      CALL MINIT ('WRIT', LUNWRK, FIND3, NX, NY, WIN, OBUFF, BUFSZ3,
     *   BOWRK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                        Loop thru map
      DO 285 I = 1,NY
C                                       Read Residuals
         CALL MDISK ('READ', LUNRES, FIND1, RESBUF, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1080) IERR,I
            GO TO 990
            END IF
C                                       Read row from HAT file.
         CALL MDISK ('READ', LUNHAT, FIND2, HATBUF, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR,I
            GO TO 990
            END IF
C                                       Write file back to disk.
         CALL MDISK ('WRIT', LUNWRK, FIND3, OBUFF, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR, I
            GO TO 990
            END IF
C                                        Find new map estimate
         CALL RFILL (NX, 0.0, OBUFF(BIND3))
         DO 284 J = 1,NX
            DO 135 IBOX = 1,NBOXS
               IF ((J.GE.WINM(1,IBOX)) .AND. (J.LE.WINM(3,IBOX)) .AND.
     *            (I.GE.WINM(2,IBOX)) .AND. (I.LE.WINM(4,IBOX))) THEN
                  IW = BIND1+J-1
                  IG = BIND2+J-1
                  IO = BIND3+J-1
C
C                                        Case to correct code.
C                                        HATBUF is HAT, WBUF is RES.
                  OBUFF(IO) = HATBUF(IG)
                  ITEMP = ABS (ICODE)
C                                        TUNNEL ALGORITHM
                  IF (ITEMP.EQ.1) THEN
                     WEIGHT = APARM(2) +
     *               HATBUF(IG)**2/(HATBUF(IG)**2 + SIGMA2)
C                                        WEIGHT ALGORITHM
                  ELSE IF (ITEMP.EQ.2) THEN
                     WEIGHT = ALPHA*(RESBUF(IW)/RMAX)**2
                     WEIGHT = WEIGHT+BETA*(HATBUF(IG)/HMAX)**2
C                                        VAN CITTERT ALGORITHM
                  ELSE IF (ITEMP.EQ.3) THEN
                     WEIGHT = 1.
C                                        STEER ALGORITHM
                  ELSE IF (ITEMP.EQ.4) THEN
                     WEIGHT = 0.
                     IF (TRIM.LE.ABS(RESBUF(IG))) WEIGHT = 1.
C                                        PERCENT ALGORITHM
                  ELSE IF (ITEMP.EQ.5) THEN
                     WEIGHT = (RESBUF(IG)/RESRMS)**2
                     WEIGHT = WEIGHT/(APARM(3) + WEIGHT)
                     WEIGHT = (ALPHA+BETA*(RESBUF(IG)/RMAX)**2)*WEIGHT
                     END IF
C                                        Add the weighted residual.
C                                       Clip asnec.
                  OBUFF(IO) = OBUFF(IO) + WEIGHT*GAINS*RESBUF(IW)
                  IF ((ICODE.GT.0) .AND. (OBUFF(IO).LT.0)) OBUFF(IO)=0.
C                                        Collect statistics.
                  HATMAX = MAX (HATMAX, OBUFF(IO))
                  HATMIN = MIN (HATMIN, OBUFF(IO))
                  ERRRMS = ERRRMS + (OBUFF(IO)-HATBUF(IG))**2
                  RNXY = RNXY + 1.0
                  GO TO 284
                  END IF
 135           CONTINUE
 284        CONTINUE
 285     CONTINUE
C                                       End of this pass, close files.
      CALL ZCLOSE (LUNRES, FIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1150) IERR
         CALL MSGWRT (8)
         END IF
      CALL ZCLOSE (LUNHAT, FIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1160) IERR
         CALL MSGWRT (8)
         END IF
C                                       Finish writing HAT file.
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
C                                        Output rms CHANGE
      ERRRMS = SQRT (ERRRMS/RNXY)
      WRITE (MSGTXT,1220) ERRRMS
      CALL MSGWRT (4)
C                                       Switch WRK and HAT files.
      CALL FSWTCH (HATFIL, WRKFIL, HATVOL, WRKVOL, CNOHAT, CNOWRK,
     *   BOHAT, BOWRK)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('VCING: ERROR ',I3,' OPENING FILE ')
 1020 FORMAT ('VCING: ERROR ',I3,' INIT RES FILE')
 1030 FORMAT ('VCING: ERROR ',I3,' OPENING HAT FILE ')
 1050 FORMAT ('VCING: ERROR ',I3,' INIT HAT FILE')
 1090 FORMAT ('VCING: WRITE ERROR ',I3,' HAT ROW ',I6)
 1080 FORMAT ('VCING: ERROR ',I3,' READ RES ROW ',I6)
 1100 FORMAT ('VCING: ERROR ',I3,' READ HAT ROW ',I6)
 1150 FORMAT ('VCING: ERROR ',I3,' CLOSING RES FILE')
 1160 FORMAT ('VCING: ERROR ',I3,' CLOSING HAT FILE')
 1170 FORMAT ('VCING: FINISH ERROR ',I3,' WRITING HAT FILE ')
 1220 FORMAT ('  CORRECTION    : RMS CHANGE   = ',1PE12.4)
      END
      SUBROUTINE FLAT (RMAX, IERR)
C-----------------------------------------------------------------------
C   FLAT INITIALISES the estimate of the VC map.  The new estimate is
C   written into the HAT file.
C   Programmer : T.J. Cornwell September 1982
C-----------------------------------------------------------------------
      REAL   RMAX
      INTEGER   IERR
      INTEGER   FIND3, WIN(4), BIND3, I, J, IBOX, IO
      LOGICAL   MAP, EXCL, WAIT
      REAL      WBUFF(1), GBUFF(1), OBUFF(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (WBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      EQUIVALENCE (OBUFF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,2*.TRUE./
C-----------------------------------------------------------------------
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                       Open and INIT HAT file for write
      CALL ZOPEN (LUNHAT, FIND3, HATVOL, HATFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            GO TO 990
            END IF
      CALL MINIT ('WRIT', LUNHAT, FIND3, NX, NY, WIN, OBUFF, BUFSZ3,
     *   BOHAT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                        Set to be FLAT
      DO 150 J = 1,NY
C                                       Write file back to disk.
         CALL MDISK ('WRIT', LUNHAT, FIND3, OBUFF, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR, J
            GO TO 990
            END IF
         CALL RFILL (NX, 0.0, OBUFF(BIND3))
         DO 140 I = 1,NX
            DO 135 IBOX = 1,NBOXS
               IF ((I.GE.WINM(1,IBOX)) .AND. (I.LE.WINM(3,IBOX)) .AND.
     *            (J.GE.WINM(2,IBOX)) .AND. (J.LE.WINM(4,IBOX))) THEN
                  IO = BIND3+I-1
                  OBUFF(IO) = RMAX
                  GO TO 140
                  END IF
 135           CONTINUE
 140        CONTINUE
 150     CONTINUE
C                                       Perform enema
      CALL MDISK ('FINI', LUNHAT, FIND3, OBUFF, BIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
C                                        Close file
      CALL ZCLOSE (LUNHAT, FIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1160) IERR
         CALL MSGWRT (8)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('FLAT: ERROR ',I3,' OPENING HAT FILE ')
 1050 FORMAT ('FLAT: ERROR ',I3,' INIT HAT FILE')
 1090 FORMAT ('FLAT: WRITE ERROR ',I3,' HAT ROW ',I6)
 1100 FORMAT ('FLAT: ERROR ',I3,' FLUSHING HAT FILE')
 1160 FORMAT ('FLAT: ERROR ',I3,' CLOSING HAT FILE')
      END
      SUBROUTINE DISPTV (TVPASS)
C-----------------------------------------------------------------------
C   DISPTV displays the current map on the TV, showing inner
C   portion only if that's all that will fit.
C   Inputs:  TVPASS  I     code: 0 => clear screen, else don't
C                                0,1 => don't question the user about
C                                quitting
C   Output:  TVPASS  I     code: 32700 => user wants to quit VSing
C-----------------------------------------------------------------------
      CHARACTER  PREFIX*5
      INTEGER   TVPASS, JROW(1), WIN(4), MX2, MY, FIND, BIND, IERR, ICH,
     *   CATBLK(256), S2H(256), IQ, IB, I, MX, INC(2), MV
      INTEGER   IWIN(4), IY
      REAL      XN(4), CATR(256), TD, RPOS(2), XFMIN
      LOGICAL   MAP, EXCL, WAIT, LERR, F
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'APVC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVD.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (JROW(1), BUFF2(1))
      EQUIVALENCE (S2H, CATR, CATD)
      DATA MAP, EXCL, WAIT / .TRUE., 2*.TRUE./
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      ICH = 1
      CALL TVOPEN (BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
      IF (TVPASS.EQ.0) THEN
         CALL TVSET (ICH, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 998
            END IF
         END IF
      TVFMAX = HATMAX
      XN(1) = 0.0
      XN(2) = TVFMAX
      XN(3) = (MAXINT - 0.02) / TVFMAX
      XN(4) = 0.51
      CALL QPUT (XN, 0, 4, 2)
C                                       Write scaling factor
      XFMIN = TVFMAX
      CALL METSCA (XFMIN, PREFIX, LERR)
      WRITE (MSGTXT,1020) XFMIN, PREFIX
      CALL MSGWRT (2)
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
      CALL ZOPEN (LUNHAT, FIND, HATVOL, HATFIL, MAP, EXCL, WAIT, IERR)
      CALL MINIT ('READ', LUNHAT, FIND, NX, NY, WIN, BUFF1, BUFSZ1,
     *   BOHAT, IERR)
      MX = WIN(3) - WIN(1) + 1
      MV = IWIN(3) - IWIN(1) + 1
      MX2 = MV
      MY =  WIN(4) - WIN(2)+ 1
C                                       loop, passing map to IIS.
      IY = IWIN(2) - 1
      DO 100 I = 1,MY
         CALL MDISK ('READ', LUNHAT, FIND, BUFF1, BIND, IERR)
         IF (IERR.NE.0) GO TO 110
         IF (MOD(I-1, INC(2)).EQ.0) THEN
            CALL QPUTIN (BUFF1(BIND), INC(1), 4, 1, MV, 2)
            CALL QWD
            CALL QVCLIP (4, 1, 0, 1, 4, 1, MV)
            CALL QVSMSA (4, 1, 2, 3, 4, 1, MV)
            CALL QVFIX (4, 1, 4, 1, MV)
            CALL QWR
            CALL QGET (JROW, 4, MV, 1)
            CALL QWD
C                                       Send row to IIS.
            IY = IY + 1
            CALL YIMGIO ('WRIT', ICH, IWIN, IY, 0, MX2, JROW, IERR)
            IF (IERR.NE.0) GO TO 110
            END IF
 100     CONTINUE
 110  CALL ZCLOSE (LUNHAT, FIND, IERR)
C                                       Image catalog
      CALL COPY (256, CATBLK, S2H)
      S2H(IIVOL) = 0
      S2H(IICNO) = 0
      CALL FILL (5, 1, S2H(IIDEP))
      CALL COPY (4, IWIN, S2H(IICOR))
      CALL COPY (4, WIN, S2H(IIWIN))
      CALL CHR2H (2, '  ', 1, S2H(IITRA))
      CATR(IRRAN) = 0.0
      CATR(IRRAN+1) = TVFMAX
      CATR(KRDMN) = 0.0
      CATR(KRDMX) = TVFMAX
      CALL YCWRIT (ICH, IWIN, S2H, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110)
         CALL MSGWRT (6)
         END IF
C                                       Ask user to quit?
      IF (TVPASS.GE.2) THEN
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
            IF (IERR.NE.0) GO TO 135
C                                       Wants to quit
            IF (IB.GT.7) THEN
               TVPASS = 32700
               MSGTXT = 'TV Button D hit: have done enough I guess'
               CALL MSGWRT (4)
               GO TO 150
               END IF
            IF (IB.GT.0) GO TO 135
 130        CONTINUE
 135     MSGTXT = 'Continuing'
         CALL MSGWRT (1)
C                                       Off cursor
 150     CALL YCURSE ('OFFF', F, F, RPOS, IQ, IB, IERR)
         END IF
C
 998  CALL TVCLOS (BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANT OPEN TV IER=',I6)
 1010 FORMAT ('IMCLEAR ERROR =',I6)
 1020 FORMAT ('TVDISP: DISPLAY MAXIMUM =',F8.3,1X,A5,'JY')
 1070 FORMAT ('Loading TV from',1PE10.3,' to',1PE10.3,' every',I2,
     *   ' pixel')
 1110 FORMAT ('CAN''T UPDATE IMAGE CATALOG  IER=',I6)
      END
