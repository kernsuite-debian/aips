LOCAL INCLUDE 'STEER.INC'
C                                                          Include STEER
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for STEER
      DOUBLE PRECISION PTRJ, PTRK
      REAL   SCRAT(6), GAIN, FMIN, FLUX, RESMAX, MAPLIM, GAUSA, GAUSB,
     *   GAUSC, GAUSAA, GAUSBB, GAUSCC, ALPHA, FITRMS, XSPACE, YSPACE,
     *   MAPROT, SPEXP, TVFMAX, GAMMA, BETA, BMOFF,
     *   BUFF1(MABFSS), BUFF2(MABFSS), BUFF3(MABFSS)
      INTEGER   NX, NY, NUMBIN, GSLST, GSSTR,
     *   NBOXS, WINM(4,10), GSLIM, BPS, LUNBEM, WINB(4), LUNHAT, LUNRES,
     *   LUNRS1, LUNWT, LUNGD1, LUNGD2, LUNDRT, LUNWRK, LUNCL1, LUNCL2,
     *   BEMVOL, RESVOL, WTVOL, GRDVOL, DRTVOL, WRKVOL, GSVOL, HATVOL,
     *   BOBEM, BORES, BOWT, BOGRD, BODRT, BOWRK, BOHAT,
     *   CNORES, CNOWT, CNOGRD, CNOWRK, CNOHAT, ICENX, ICENY,
     *   HATSCR, BUFSZ1, BUFSZ2, BUFSZ3
      CHARACTER BEMFIL*48, RESFIL*48, HATFIL*48, WTFIL*48, GRDFIL*48,
     *   DRTFIL*48, WRKFIL*48, GSFIL*48
      COMMON /STCOM/ PTRJ, PTRK, ALPHA, GAIN, FMIN,
     *   FLUX, RESMAX, SCRAT, MAPLIM, GAUSA, GAUSB, GAUSC, GAUSAA,
     *   GAUSBB, GAUSCC, XSPACE, YSPACE, MAPROT,
     *   SPEXP, TVFMAX, ICENX, ICENY, NX, NY, NBOXS,
     *   WINM, NUMBIN, GAMMA, BETA, FITRMS, BMOFF, GSLST, GSSTR, GSLIM,
     *   WINB, BPS, LUNBEM, LUNRES, LUNRS1, LUNWT, LUNGD1, LUNGD2,
     *   LUNDRT, LUNWRK, LUNCL1, LUNCL2, LUNHAT, BEMVOL, RESVOL, WTVOL,
     *   GRDVOL, DRTVOL, WRKVOL, GSVOL, HATVOL,
     *   BOBEM, BORES, BOWT, BOGRD, BODRT,
     *   BOWRK, BOHAT, CNORES, CNOWT, CNOGRD, CNOWRK, CNOHAT, HATSCR
      COMMON /STCHR/ HATFIL, BEMFIL, RESFIL, WTFIL, GRDFIL, DRTFIL,
     *   WRKFIL, GSFIL
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3,
     *   BUFSZ1, BUFSZ2, BUFSZ3
C                                                          End STEER
LOCAL END
      PROGRAM STEER
C-----------------------------------------------------------------------
C! Does Steer deconvolution
C# Map AP-appl
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
C   STEER makes a map using the algorithm of David Steer DRAO.  It is
C   an AIPS task in this implementation.
C     Adverb    Alias   Dim(R)   Loc            Comments
C     INNAME    NAME(1,1) 3       1     Dirty map name
C     INCLASS   CLASS(1,1)2       4     Dirty map class
C     INSEQ     SEQ(1)    1       6     Dirty map sequence number
C     INDISK    VOL(1)    1       7     Dirty map volumn
C     IN2NAME   NAME(1,2) 3       8     Beam map name
C     IN2CLASS  CLASS(1,2)2      11     Beam map class
C     IN2SEQ    SEQ(2)    1      13     Beam map sequence number
C     IN2DISK   VOL(2)    1      14     Beam map volumn
C     OUTNAME   NAME(1,3) 3      15     GS map name
C     OUTCLASS  CLASS(1,3)2      18     GS map class
C     OUTSEQ    SEQ(3)    1      20     GS map sequence number
C     OUTDISK   VOL(3)    1      21     GS map volumn
C     GAIN      GAIN      1      22     GS loop gain
C     NITER     GSLIM     1      23     Max number of GS iterations
C     DOTV      ITV       1      24     1 = request TV display of GS
C                                       maps and ask user if continue,
C                                       < 0 => no display or question
C     NBOXES    NBOXS     1      25     Number of boxes for CLEAN
C     BOX(4,10) WINM     40      65     Windows for the CLEAN boxes.
C     BADDISK   IBAD     10      75     Bad disk list.
C     OFFSET    BMOFF     1      76     Offset to beam
C   Programmer = T. J. Cornwell Feb 1984
C-----------------------------------------------------------------------
      INTEGER   TVPASS, I
      INTEGER   CORN(7), JWIN(4)
      INTEGER   USID, VOL(3), SEQ(3), ITV, CATBLK(256), ITER, NEED,
     *   IRET, IERR, IDIR, IER, APKEY
      LOGICAL   FINISH, OLD, T, WASGS, EQUAL
      REAL      CATR(256), RMAX, RMIN, RRSMAX, RRSMIN
      CHARACTER SCRFIL*48, CLASS(3)*6, NAME(3)*12
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DAPM.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATR(1), CATBLK(1))
      DATA CORN, JWIN /7*1, 4*0/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Release AP memory
      CALL QRLSE
      ITER = 0
      NCFILE = 0
      NSCR = 0
      TVPASS = 0
      FINISH = .FALSE.
      WASGS = .FALSE.
C                                       Get input values.
      CALL GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IRET)
C                                       Check for restart of AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Get map files and create
C                                       output files if required.
      CALL FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (OLD) THEN
         GSSTR = CATBLK(KINIT)
         IF (GSSTR.GT.0) ITER = GSSTR - 1
         WRITE (MSGTXT,1000) GSSTR
         CALL MSGWRT (4)
         END IF
C                                       Write inputs to history and
C                                       log files.
      OLD = (OLD) .AND. (GSSTR.GT.0)
      CALL GSHIS (NAME, VOL, CLASS, SEQ, OLD)
C
      CALL ADDBEM (IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Initialise GS map
      IF (.NOT.OLD) CALL FLAT (CATR(KRDMX), CATR(KRDMN), IRET)
      IF (IRET.NE.0) GO TO 990
C
      RESMAX = CATR(KRDMX)
C                                       Get ap memory
      IDIR = 1
      NEED = 4 * NY * (NX/2 + 1)
      NEED = NEED / 1024
      MSGSUP = 32000
      CALL QINIT (NEED, 0, APKEY)
      MSGSUP = 0
      IF ((APKEY.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = (NY * (NX/2 + 1)) / 2
         NEED = NEED / 1024
         NEED = MIN (16 * 1024, NEED) + 4
         CALL QINIT (NEED, 0, APKEY)
         IF ((APKEY.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'CANNOT GET NEEDED MEMORY'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 990
            END IF
         END IF
C                                       Display current map.
      IF (ITV.EQ.1) CALL DISPTV (TVPASS)
C                                       Make WEIGHT file.
      CALL APDFFT (RMAX, RMIN, IDIR, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL QRLSE
C                                       Begin GSing.
      ITER = GSSTR
      IF (.NOT.FINISH) THEN
         ITER = ITER + 1
         WRITE (MSGTXT,1100) ITER
         CALL MSGWRT (4)
C                                       Create RES file
C                                        This is the current GS
C                                       convolved with the beam
         CALL QINIT (0, 0, APKEY)
         IF ((APKEY.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'UNABLE TO GET AP MEMORY BACK'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 990
            END IF
         CALL CNVLVE (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Subtract RES from DRT
C                                        to obtain the RES file
         CALL SUBMAP (RRSMAX, RRSMIN, IRET)
         IF (IRET.NE.0) GO TO 990
C                                        Using the HAT,RES files
C                                        calculate the new GS map
C                                        and store it in the HAT file
         CALL ASTEER (RRSMAX, RRSMIN, CATR(KRDMX), CATR(KRDMN), IRET)
         IF (IRET.NE.0) GO TO 990
         IF (ITER.GE.GSLIM) FINISH = T
C                                       Determine RESMAX
         RESMAX = CATR(KRDMX)
C                                       Display current RES map.
         IF (FINISH) TVPASS = MIN (TVPASS, 1)
         IF (ITV.EQ.1) CALL DISPTV (TVPASS)
         CALL QRLSE
         IF (TVPASS.EQ.32700) FINISH = .TRUE.
         TVPASS = 2
C                                       If more GSing required,
C                                       loop.
         END IF
C                                       Update GS catalog header
      CATBLK(KINIT) = ITER
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KITYP) = 4
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
      WRITE (MSGTXT,1300)
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
 1000 FORMAT ('RESTARTING GSING AFTER ',I5,' ITERATIONS')
 1100 FORMAT ('ITERATION = ',I5)
 1200 FORMAT ('ERROR',I3,' UPDATING GS HEADER ')
 1300 FORMAT ('I SEEM TO HAVE LOST THE OUTPUT FILE')
      END
      SUBROUTINE SUBMAP (RMAX, RMIN, IERR)
C-----------------------------------------------------------------------
C   SUBMAP subtracts the RES file ( = beam * map ) from the Dirty map
C   producing the residual map.
C   The WORK file is used as the output file and then
C   the physical files for the Residual and WORK files are
C   exchanged when the routine is finished.
C   OUTPUT:
C     RMAX    R   Maximum in residual map.
C     RMIN    R   Minimum in residual map.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, WIN(4),
     *   IERR, I, J, IBOX, IO, IG, ID
      LOGICAL   MAP, EXCL, WAIT
      REAL      RMAX, RMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA MAP, EXCL, WAIT /.TRUE., 2*.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize extrema.
      RMAX = -1.0E20
      RMIN =  1.0E20
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
            WRITE (MSGTXT,1070) IERR,J
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
                  IO = BIND3 + J - 1
                  IG = BIND2 + J - 1
                  ID = BIND1 + J - 1
                  BUFF3(IO) = BUFF1(ID) - BUFF2(IG)
                  RMAX = MAX(BUFF3(IO), RMAX)
                  RMIN = MIN(BUFF3(IO), RMIN)
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
C                                       Switch RES and WRK files.
      CALL FSWTCH (RESFIL, WRKFIL, RESVOL, WRKVOL, CNORES, CNOWRK,
     *   BORES, BOWRK)
      IERR = 0
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
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
      SUBROUTINE ADDBEM (IERR)
C-----------------------------------------------------------------------
C     Finds the correct scaling factor for the beam. Adds all points
C     greater than BMOFF in the beam. Answer put into GAMMA in Common.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      INTEGER   FIND1, BIND1, WIN(4), IERR, I, J, IBOX, ID
      LOGICAL   MAP, EXCL, WAIT
      REAL      TEMP, BEMMAX, BEMMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA MAP, EXCL, WAIT /.TRUE., 2*.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize extrema.
      BEMMAX = -1E20
      BEMMIN =  1E20
      GAMMA = 0.0
C                                       Open and INIT files.
      CALL ZOPEN (LUNBEM, FIND1, BEMVOL, BEMFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      CALL MINIT ('READ', LUNBEM, FIND1, NX, NY, WIN, BUFF1, BUFSZ1,
     *   BOBEM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Loop thru BEAM
      DO 150 I = 1,NY
C                                       Read dirty map.
         CALL MDISK ('READ', LUNBEM, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR, J
            GO TO 990
            END IF
         DO 140 J = 1,NX
            DO 135 IBOX = 1,NBOXS
               IF ((J.GE.WINM(1,IBOX)) .AND. (J.LE.WINM(3,IBOX)) .AND.
     *            (I.GE.WINM(2,IBOX)) .AND. (I.LE.WINM(4,IBOX))) THEN
                  ID = BIND1 + J - 1
                  TEMP = BUFF1(ID)
                  IF (TEMP.GT.BMOFF) GAMMA = GAMMA + TEMP
                  BEMMAX = MAX(BEMMAX, TEMP)
                  BEMMIN = MIN(BEMMIN, TEMP)
                  GO TO 140
                  END IF
 135           CONTINUE
 140        CONTINUE
 150     CONTINUE
C                                       Close files.
      CALL ZCLOSE (LUNBEM, FIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1160) IERR
         GO TO 990
         END IF
C
      WRITE (MSGTXT,2000) GAMMA
      CALL MSGWRT (4)
      WRITE (MSGTXT,2100) BEMMAX,BEMMIN
      CALL MSGWRT (4)
C                                       If GAMMA = 0 something wrong
      IF (GAMMA.EQ.0.0) THEN
         IERR = 15
         WRITE (MSGTXT,1170)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ADDBEM: CANNOT OPEN BEAM FILE, ERROR ',I3)
 1040 FORMAT ('ADDBEM: CANNOT INIT BEAM, ERROR ',I3)
 1070 FORMAT ('ADDBEM: READ ERROR ',I3,' BEAM FILE ROW ',I6)
 1160 FORMAT ('ADDBEM: CLOSE ERROR ',I3,' BEAM MAP ')
 1170 FORMAT ('ADDBEM: NO BEAM VALUES ABOVE CUTOFF')
 2000 FORMAT ('   SUM OF BEAM = ',1PE11.3)
 2100 FORMAT ('   MAX,MIN OF BEAM = ',2(1X,1PE11.3))
      END
      SUBROUTINE APDFFT (SMAX, SMIN, IDIR, IERR)
C-----------------------------------------------------------------------
C   APDFFT calls AP FFT routines PASS1 and PASS2.  If the transform
C   can be done all in the AP this is done; if not then a transpose
C   of the type designed by F. Schwab is done.
C   The work file is used in
C   the transpose between passes of the transform.
C   The beam is assumed in file GRD.
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
C     BOHAT      I    Block offset for GS map.
C     HATVOL     I    Volume for GS map.
C     HATFIL     C    Physical name of GS map.
C     LUNHAT     I    LUN for GS map.
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
      REAL   SMAX, SMIN
      LOGICAL   F
      CHARACTER  FIL(3)*48
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       REAL TO REAL
C                                       Set file information.
C                                       1 = Beam, 2 = Work, 3 = Weight
      IF ((IDIR.GE.0) .AND. (IDIR.NE.2)) THEN
         IDIRT = IDIR
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
      SUBROUTINE GSHIS (NAME, VOL, CLASS, SEQ, OLD)
C-----------------------------------------------------------------------
C   GSHIS copies the dirty map history if the GS map does not
C   already have a history file.  Then the inputs to STEER with
C   the default values are added to the history file.
C   INPUTS:
C     NAME(6,3)   C*12?Names of maps
C                      1 = Dirty map
C                      2 = Beam map
C                      3 = GS map
C     VOL(3)      I    Vol. numbers of the maps.
C     CLASS(3)    I    Classes of the map.
C     SEQ(3)      I    Sequence numbers of the maps.
C     OLD         L    T => GS map pre-exists & restart > 0
C   From common /MAPHDR/
C     CATBLK(256) I    Catalog header block for the GS map.
C   From COMMON /GS/
C     NX          I    Number of cell in the map in RA.
C     NY          I    Number of cells in the map in dec.
C     GSLIM      I    Max number of iterations.(NITER)
C     GSSTR      I    Numb. of previous GS comp. to use.
C     GAIN        R    GS loop gain.
C     FMIN        R    Min. GS residual.
C     BUFF1()    I    Work buffer
C     BUFF2()    I    Work buffer
C     LUNCL1      I    LUN for GS history file
C     LUNDRT      I    LUN for dirty history file
C     GSVER      I    GS comp. file version number.
C   From COMMON /CFILES/
C     FCNO(10)    I    Catalog slot numbers of catalogd maps.
C                      2 = Dirty map
C                      3 = GS map
C   OUTPUT:
C     The input parameters are written on the history file and the
C     log file.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      CHARACTER NAME(3)*12, CLASS(3)*6, XCLS*8, XNAM*12
      INTEGER   VOL(3), SEQ(3), CATBLK(256), IERR, I, J
      LOGICAL   T, OLD
      CHARACTER HILINE*72
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'STEER.INC'
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
 30   WRITE (MSGTXT,2000) TSKNAM,NAME(1),CLASS(1),SEQ(1),VOL(1)
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
C                                       GS map name.
      WRITE (MSGTXT,2002) TSKNAM,NAME(3),CLASS(3),SEQ(3),VOL(3)
      CALL MSGWRT (3)
      CALL HENCOO (TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Maximum number of Iterations.
      WRITE (HILINE,2003) TSKNAM, GSLIM
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Loop gain.
      WRITE (HILINE,2005) TSKNAM, GAIN
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       offset.
      WRITE (HILINE,2006) TSKNAM, BMOFF
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
 1010 FORMAT ('UNABLE TO OPEN OLD GS-MAP HISTORY FILE')
 1020 FORMAT ('GSHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT ('GSHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT (A6,' DIRTY MAP =''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2001 FORMAT (A6,' BEAM MAP = ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2002 FORMAT (A6,' GS MAP =''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2003 FORMAT (A6,' NITER = ',I6,' /MAX. NO. GS ITERATIONS')
 2005 FORMAT (A6,' GAIN = ',F7.4,' /GS LOOP GAIN FACTOR')
 2006 FORMAT (A6,' OFFSET = ',F7.4,' /TRIM LEVEL')
 2009 FORMAT (A6,' NBOXES = ',I3,' /NO. GS WINDOWS')
 2010 FORMAT (A6,' BOX(',I2,') = ',I5,', ',I5,', ',I5,', ',I5,
     *   ' /GS WINDOW')
      END
      SUBROUTINE FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   FILES reads and floats the  beam and creates the
C   GS map and components files. The R   beam is placed in the
C   GRD file and the location of the R   catalogd files are
C   left in the DRT and BEM files.
C   BEAM and Dirty maps in catalog are marked READ and GS map
C   is marked WRITE.
C   INPUT:
C     USID     I    User number.
C     NAME(6,3)  I    Names of maps
C                     1 = Dirty map
C                     2 = Beam map
C                     3 = GS map
C     CLASS(3,3) I    Classes of the maps
C     VOL(3)     I    Volumns of the maps.
C   OUTPUT:
C     ICENX    I    Offset in X of BEAM center from NX/2+1
C     ICENY    I    Offset in Y of BEAM center from NY/2+1
C     OLD      L    TRUE if old map file being replaced.
C   Commons:
C     CATBLK(256) in /MAPHDR/ a preliminary header for the GS map
C        is returned.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      CHARACTER  NAME(3)*12, CLASS(3)*6, STAT*4, MAPUNT*8, MTYPE*2
      INTEGER   USID, CATBLK(256), CNO, IROUND,
     *   VOL(3), SEQ(3), OLDIM, OLDNAX(7), IDEPTH(5),
     *   IERR, IBOX, IER, I, IMCENX, IMCENY, NREC, LREC
      INTEGER   CORN(7), JWIN(4), NOSCR, ISIZE
      LOGICAL   OLD
      DOUBLE PRECISION CATD(128), OLDCRV(7)
      REAL      CATR(256), XMAX, XMIN, XCEN, YCEN
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      DATA CORN, JWIN, IDEPTH /7*1, 4*0, 5*1/
      DATA MAPUNT /'JY/PIXEL'/
C-----------------------------------------------------------------------
      NCFILE = 0
C                                       Read dirty beam.
C                                       Get catalog slot for BEAM.
      CNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', VOL(2), CNO, NAME(2), CLASS(2), SEQ(2),
     *   MTYPE, USID, STAT, BUFF3, IERR)
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
      WRITE (MSGTXT,1777) XCEN, YCEN
      CALL MSGWRT (4)
      XCEN = XCEN - NX/2 - 1
      YCEN = YCEN - NY/2 - 1
      ICENX = IROUND (XCEN)
      ICENY = IROUND (YCEN)
C                                       Create scratch files.
C                                       Residual file
      NSCR = 0
      ISIZE = MAX (NX, 64) + 3
      ISIZE = (ISIZE * NY) * 4 + 2048
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
C                                       Read beam map.
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
     *   MTYPE, USID, STAT, BUFF3, IERR)
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
C                                       Warn if "GS"
      IF (CATBLK(KITYP).EQ.4) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (4)
         END IF
C                                       Get dirty max,min.
C                                       Set info into COMMON.
      XMAX = CATR(KRDMX)
      XMIN = CATR(KRDMN)
      RESMAX = MAX (ABS(XMAX) , ABS(XMIN))
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
      CALL CHR2H (12, NAME(3), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASS(3), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQ(3)
C                                        Put in units of map
      CALL CHR2H (8, MAPUNT, 1, CATH(KHBUN))
C                                       Create output GS map file.
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
               IF (OLDCRV(I).NE.CATD(KDCRV+I-1)) GO TO 215
 210           CONTINUE
      IMCENX = 0
      IMCENY = 0
      CALL PLNGET (VOL(3), CNO, CORN, JWIN, IMCENX, IMCENY, HATSCR, NX,
     *   NY, BUFF1, BUFF2, BUFSZ1, BUFSZ2, LUNGD1, LUNDRT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1038) IERR
         GO TO 990
         END IF
C                                       It matches, mark WRITE
      CALL CATIO ('WRIT', VOL(3), CNO, CATBLK, 'WRIT', BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1210) IERR
         CALL MSGWRT (7)
         GO TO 300
         END IF
C                                       Not same file !!!
 215  IERR = 8
      WRITE (MSGTXT,1215)
      GO TO 990
C                                       Actual seq #
 300  SEQ(3) = CATBLK(KIIMS)
C                                       Add to /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(3)
      GSVOL = VOL(3)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
      IF (OLD) FRW(NCFILE) = 1
      CCNO = CNO
      NREC = GSLIM + 10
      LREC = 3
      IERR = 0
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
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
 1100 FORMAT ('WARNING: MAY BE GSING A GS MAP')
 1105 FORMAT ('FILES: UNEQUAL DIMENSIONS IN DIRTY AND BEAM MAPS')
 1106 FORMAT ('       BEAM =',2I5,' DIRTY =',2I5)
 1190 FORMAT ('FILES: COULD NOT CREATE GS MAP FILE, ERROR ',I3)
 1210 FORMAT ('FILES: CANNOT UPDATE GS CATBLK, ERROR',I3)
 1215 FORMAT ('OLD GS MAP NOT COMPATIBLE WITH DIRTY MAP')
 1777 FORMAT ('    PEAK OF BEAM IS AT ',2(F6.0,1X))
      END
      SUBROUTINE GETIN (USID, NAME, VOL, CLASS, SEQ, ITV, IERR)
C-----------------------------------------------------------------------
C   GETIN gets the input parameters for the program from AIPS
C  See Prologue for STEER for more details.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      HOLLERITH XNAM1(3), XCLAS1(2), XNAM2(3), XNAM3(3), XCLAS2(2),
     *   XCLAS3(2)
      CHARACTER PRGNAM*6, NAME(3)*12, CLASS(3)*6, DEFCLS*6, MTYPE*2
      INTEGER   USID, SEQ(3), VOL(3), ITV, IL, I, IK, J, CNO, I4TEMP
      INTEGER   NPARMS, IERR, CATBLK(256), IND, IROUND
      REAL      XSEQ1, XSEQ2, XSEQ3, XVOL1, XVOL2, XVOL3, XG,
     *   XNI, XDOTV, XNB, XBOX(4,10), XBAD(10), XOFF
      LOGICAL   T, F, EQUAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XVOL1, XNAM2, XCLAS2,
     *   XSEQ2, XVOL2, XNAM3, XCLAS3, XSEQ3, XVOL3, XG, XNI,
     *   XDOTV, XNB, XBOX, XBAD, XOFF
      DATA PRGNAM /'STEER '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize common parameters.
C                                        global areas
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      CALL HIINIT (3)
C                                        non-standard messages
C                                        STEER common areas
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
      BMOFF = 0.5
      GSLIM = 20
      GSSTR = 0
      FITRMS = 1.E20
      FMIN = 0.001
      GAIN = 0.50
      NUMBIN = 1024
      I4TEMP = 2  * MABFSS
      BUFSZ1 = I4TEMP
      BUFSZ2 = I4TEMP
      BUFSZ3 = I4TEMP
      BPS = NBPS
      TVFMAX = 0.0
C                                       Get AIPS adverbs.
      NPARMS = 76
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
      CALL YTVCIN
C                                       Convert characters
      CALL H2CHR (12, 1, XNAM1, NAME(1))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (12, 1, XNAM2, NAME(2))
      CALL H2CHR (6, 1, XCLAS2, CLASS(2))
      CALL H2CHR (12, 1, XNAM3, NAME(3))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
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
C                                       Default name for Beam and GS
C                                       maps is Dirty map name
      EQUAL = NAME(2) .EQ. '            '
      IF (EQUAL) NAME(2) = NAME(1)
C                                       Copy CLASSes or default.
      EQUAL = CLASS(2) .EQ. '      '
      IF (EQUAL) THEN
         CLASS(2) = 'IBEM  '
         IF (CLASS(1).EQ.'RMAP  ') CLASS(2) = 'RBEM  '
         IF (CLASS(1).EQ.'LMAP  ') CLASS(2) = 'LBEM  '
         END IF
      DEFCLS = 'IST   '
      IF (CLASS(1).EQ.'RMAP  ') DEFCLS = 'RST   '
      IF (CLASS(1).EQ.'LMAP  ') DEFCLS = 'LST   '
      IF (CLASS(1).EQ.'QMAP  ') DEFCLS = 'QST   '
      IF (CLASS(1).EQ.'UMAP  ') DEFCLS = 'UST   '
      IF (CLASS(1).EQ.'VMAP  ') DEFCLS = 'VST   '
C                                       Get volumn numbers
      VOL(2) = IROUND (XVOL2)
      VOL(3) = IROUND (XVOL3)
C                                       Get sequence numbers
      SEQ(2) = IROUND (XSEQ2)
      SEQ(3) = IROUND (XSEQ3)
      IF (SEQ(2).LE.0) SEQ(2) = SEQ(1)
      CALL MAKOUT (NAME, CLASS, SEQ, DEFCLS, NAME(3), CLASS(3), SEQ(3))
C                                       Get GSing information
      IF (XG.NE.0.0) GAIN = XG
      IF (XNI.GT.0.0) GSLIM = IROUND (XNI)
      IF (XOFF.GT.0.0) BMOFF = XOFF
      ITV = 0
      IF (XDOTV.GT.0.0) ITV = 1
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
 4000 FORMAT ('YOU ARE USING A NON-STANDARD, EXPERIMENTAL PROGRAM')
      END
      SUBROUTINE CNVLVE (IERR)
C-----------------------------------------------------------------------
C   CNVLVE Fourier convolves the current map estimate with the beam.
C
C     Programmer = T. J. Cornwell May 1982
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, IERR, IDIR, JLIM, I, J, IO, IW,
     *   WIN(4), BIND1, BIND2, BIND3, IG
      REAL   RMAX, RMIN, RNXY, SMAX, SMIN, WT
      LOGICAL   MAP, EXCL, WAIT
      REAL      WBUFF(1), GBUFF(1), OBUFF(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (WBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      EQUIVALENCE (OBUFF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,2*.TRUE./
C-----------------------------------------------------------------------
C                                        Do FFT to get transform of map
      IDIR = 2
      CALL APDFFT (RMAX, RMIN, IDIR, IERR)
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
C                                       Open and INIT WRK file for write
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
         DO 140 J = 1,NY
C                                       Multiply by weights.
            IO = BIND3 + 2*(J-1)
            IW = BIND1 +  (J-1)
            IG = BIND2 + 2 * (J-1)
            WT = RNXY * WBUFF(IW)
            OBUFF(IO) = WT * GBUFF(IG)
            OBUFF(IO+1) = WT * GBUFF(IG+1)
 140        CONTINUE
 150     CONTINUE
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
      WRITE (MSGTXT,1400) SMAX,SMIN
      CALL MSGWRT (4)
      WRITE (MSGTXT,1180) IERR
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
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
 1160 FORMAT ('CNVLVE: ERROR ',I3,' CLOSING GRID FILE')
 1170 FORMAT ('CNVLVE: FINISH ERROR ',I3,' WRITING GRID FILE ')
 1180 FORMAT ('CNVLVE: ERROR ',I3,' IN APDFFT U,V TO MAP')
 1400 FORMAT ('      CONVOLVED MAP : MAX.,MIN.     = ',1PE12.4,',',
     *   1PE12.4)
      END
      SUBROUTINE ASTEER (RRSMAX, RRSMIN, HATMAX, HATMIN, IERR)
C-----------------------------------------------------------------------
C     AGS finds the next estimate of the GS map from the current
C     estimate, stored in HAT, and the residuals of the current
C     estimate, stored in RES. The new estimate is written into the
C     HAT file.
C     Programmer : T.J. Cornwell May 1982
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, IERR, IBOX, IW, IG, IO,
     *   WIN(4), BIND1, BIND2, BIND3, I, J
      LOGICAL   MAP, EXCL, WAIT
      REAL      WBUFF(1), GBUFF(1), OBUFF(1), RRSRMS,
     *   RNXY, ERRRMS, GAINS, TRIM, RRSMAX, RRSMIN, HATMAX, HATMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (WBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      EQUIVALENCE (OBUFF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,2*.TRUE./
C-----------------------------------------------------------------------
      RNXY = 0.0
      ERRRMS = 0.0
      GAINS = GAIN/GAMMA
      TRIM = BMOFF*MAX(ABS(RRSMAX),ABS(RRSMIN))
      HATMAX = -1.E20
      HATMIN =  1.E20
      RRSRMS = 0.0
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
      CALL MINIT ('READ', LUNRES, FIND1, NX, NY, WIN, WBUFF, BUFSZ1,
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
      CALL MINIT ('READ', LUNHAT, FIND2, NX, NY, WIN, GBUFF, BUFSZ2,
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
         CALL MDISK ('READ', LUNRES, FIND1, WBUFF, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1080) IERR,I
            GO TO 990
            END IF
C                                       Read row from HAT file.
         CALL MDISK ('READ', LUNHAT, FIND2, GBUFF, BIND2, IERR)
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
                  OBUFF(IO) = GBUFF(IG)
                  IF (ABS(WBUFF(IW)).GT.TRIM) OBUFF(IO) = OBUFF(IO) +
     *               GAINS*WBUFF(IW)
                  HATMAX = MAX (HATMAX, OBUFF(IO))
                  HATMIN = MIN (HATMIN, OBUFF(IO))
                  ERRRMS = ERRRMS + (OBUFF(IO)-GBUFF(IG))**2
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
C                                        Find some statistics
      WRITE (MSGTXT,1209) TRIM
      CALL MSGWRT (4)
      WRITE (MSGTXT,1210) RRSMAX, RRSMIN
      CALL MSGWRT (4)
C                                        Output rms CHANGE
      ERRRMS = SQRT (ERRRMS/RNXY)
      WRITE (MSGTXT,1400) HATMAX, HATMIN
      CALL MSGWRT (4)
      WRITE (MSGTXT,1220) ERRRMS
      CALL MSGWRT (4)
C                                       Switch WRK and HAT files.
      CALL FSWTCH (HATFIL, WRKFIL, HATVOL, WRKVOL, CNOHAT, CNOWRK,
     *   BOHAT, BOWRK)
      IERR = 0
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('AGS: ERROR ',I3,' OPENING FILE ')
 1020 FORMAT ('AGS: ERROR ',I3,' INIT RES FILE')
 1030 FORMAT ('AGS: ERROR ',I3,' OPENING HAT FILE ')
 1050 FORMAT ('AGS: ERROR ',I3,' INIT HAT FILE')
 1090 FORMAT ('AGS: WRITE ERROR ',I3,' HAT ROW ',I6)
 1080 FORMAT ('AGS: ERROR ',I3,' READ RES ROW ',I6)
 1100 FORMAT ('AGS: ERROR ',I3,' READ HAT ROW ',I6)
 1150 FORMAT ('AGS: ERROR ',I3,' CLOSING RES FILE')
 1160 FORMAT ('AGS: ERROR ',I3,' CLOSING HAT FILE')
 1170 FORMAT ('AGS: FINISH ERROR ',I3,' WRITING HAT FILE ')
 1209 FORMAT ('      RES MAP : TRIM LEVEL    = ',1PE12.4)
 1210 FORMAT ('      RES MAP : MAX.,MIN.     = ',1PE12.4,',',1PE12.4)
 1220 FORMAT ('              : R.M.S. CHANGE = ',1PE12.4)
 1400 FORMAT ('       GS MAP : MAX.,MIN.     = ',1PE12.4,',',1PE12.4)
      END
      SUBROUTINE FLAT (RMAX, RMIN, IERR)
C-----------------------------------------------------------------------
C     FLAT INITIALISES the estimate of the GS map. The new
C     estimate is written into the HAT file.
C
C     Programmer : T.J. Cornwell September 1982
C-----------------------------------------------------------------------
      INTEGER   FIND3, IERR, WIN(4), BIND3, J, I, IBOX, IO
      LOGICAL   MAP, EXCL, WAIT
      REAL      WBUFF(1), GBUFF(1), OBUFF(1), RMAX, RMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (WBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      EQUIVALENCE (OBUFF(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,2*.TRUE./
C-----------------------------------------------------------------------
      RMAX = 1.0E-08
      RMIN = 0.0
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                       Open and INIT HAT file for write
      CALL ZOPEN (LUNHAT, FIND3, HATVOL, HATFIL, MAP, EXCL, WAIT,
     *   IERR)
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
                  IO = BIND3 + I - 1
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
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1160) IERR
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
C   Output:  TVPASS  I     code: 32700 => user wants to quit GSing
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      CHARACTER  PREFIX*5
      INTEGER   TVPASS, JROW(1), WIN(4), MX2, MY, FIND, BIND, IERR, ICH,
     *   CATBLK(256), S2H(256), IQ, IB, I, IWIN(4), IY, MX, INC(2), MV
      REAL      XN(4), CATR(256), TD, RPOS(2), XFMIN
      LOGICAL   MAP, EXCL, WAIT, LERR, F
      DOUBLE PRECISION CATD(128)
      INCLUDE 'STEER.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVD.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (JROW(1), BUFF2(1))
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
      IF (TVPASS.EQ.0) THEN
         CALL TVSET (ICH, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 998
            END IF
         END IF
      IF (TVFMAX.LE.0.0) TVFMAX = RESMAX
      IF (RESMAX.GT.TVFMAX) TVFMAX = RESMAX
      IF (0.1*TVFMAX.GT.RESMAX) TVFMAX = 0.1*TVFMAX
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
      CALL CHR2H (2, '  ', 1, CATH(IITRA))
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
C                                       Off cursor
 150     CALL YCURSE ('OFF', F, F, RPOS, IQ, IB, IERR)
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
