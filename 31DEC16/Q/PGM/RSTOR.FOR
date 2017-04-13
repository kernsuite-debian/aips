LOCAL INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for RSTOR
      REAL   GAIN, FMIN, FLUX, RESMAX, MAPLIM, GAUSA, GAUSB, GAUSC,
     *   GAUSAA, GAUSBB, GAUSCC, PHAT, FACTOR, XSPACE, YSPACE, MAPROT,
     *   SPEXP, TVFMAX, TVFMIN, TVREMX, TVREMN,
     *   BUFF1(MABFSL), BUFF2(MABFSL), BUFF3(MABFSL)
      CHARACTER BEMFIL*48, RESFIL*48, WTFIL*48, GRDFIL*48,
     *   DRTFIL*48, WRKFIL*48, CLNFIL*48
      LOGICAL   NOFIT
      INTEGER   PRECLN, CLNLST, CLNSTR, CLNLIM, PTRJ, PTRK,
     *   BMHIS(129), RESHIS(1050)
      INTEGER   NX, NY, PATCH, MAXPCH, MINPCH, NUMBIN, BPS, LUNBEM,
     *   WINB(4), WINM(4,10), NBOXS, LUNRES, LUNRS1, LUNWT, LUNGD1,
     *   LUNGD2, LUNDRT, LUNWRK, LUNCL1, LUNCL2, BEMVOL, RESVOL,
     *   WTVOL, GRDVOL, DRTVOL, WRKVOL, CLNVOL, ICENX, ICENY, CLNVER,
     *   BOBEM, BORES, BOWT, BOGRD, BODRT, BOWRK, CNODRT, CNOCLN,
     *   CNORES, CNOWT, CNOGRD, CNOWRK, BLC(7), BUFSZ1, BUFSZ2, BUFSZ3
      COMMON /CLNCHR/ BEMFIL, RESFIL, WTFIL, GRDFIL, DRTFIL, WRKFIL,
     *   CLNFIL
      COMMON /CLNCOM/ BUFF1, BUFF2, BUFF3, GAIN, FMIN, FACTOR,
     *   FLUX, PHAT, RESMAX, MAPLIM, GAUSA, GAUSB, GAUSC,
     *   GAUSAA, GAUSBB, GAUSCC, XSPACE, YSPACE, MAPROT,
     *   SPEXP, TVFMAX, TVFMIN, TVREMX, TVREMN, PRECLN, CLNLST, CLNSTR,
     *   CLNLIM, PTRJ, PTRK, BMHIS, RESHIS,
     *   NOFIT,
     *   ICENX, ICENY, NX, NY, PATCH, MAXPCH, MINPCH, NUMBIN, WINB,
     *   WINM, NBOXS, BPS, LUNBEM, LUNRES, LUNRS1, LUNWT, LUNGD1,
     *   LUNGD2, LUNDRT, LUNWRK, LUNCL1, LUNCL2,
     *   BEMVOL, RESVOL, WTVOL, GRDVOL, DRTVOL, WRKVOL, CLNVOL,
     *   BOBEM, BORES, BOWT, BOGRD, BODRT, BOWRK,
     *   CNODRT, CNOCLN, CNORES, CNOWT, CNOGRD, CNOWRK, CLNVER, BLC,
     *   BUFSZ1, BUFSZ2, BUFSZ3
C                                                          End RSTOR
LOCAL END
      PROGRAM RSTOR
C-----------------------------------------------------------------------
C! Restores a CC file to a map with a gaussian beam.
C# Map AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2005-2006, 2008
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
C   RSTOR is a gutted version of APCLN.
C   INPUTS    ALIAS   DIM(R)                COMMENTS
C   INNAME    NAME(1,1) 3           Input (residual) map name
C   INCLASS   CLASS(1,1)2           Input map class
C   INSEQ     SEQ(1)    1           Input map sequence number
C   INDISK    VOL(1)    1           Input map volume
C   BLC       BLC       7           Specifies the plane to Restore.
C   IN2NAME   NAME(1,2) 3           Map name for CC file
C   IN2CLASS  CLASS(1,2)2           CC map class
C   IN2SEQ    SEQ(2)    1           CC map sequence number
C   IN2DISK   VOL(2)    1           CC map volume
C   OUTNAME   NAME(1,3) 3           Restored map name
C   OUTCLASS  CLASS(1,3)2           Restored map class
C   OUTSEQ    SEQ(3)    1           Restored map sequence number
C   OUTDISK   VOL(3)    1           Restored map volume
C   INVER     CLNVER    1           CLEAN comp. file version #.
C   NITER     CLNLIM    1           Last CLEAN component to restore.
C   BMAJ      GAUSA     1           Major axis of CLEAN restoring beam
C                                   sigma of Gaussian in cells.
C   BMIN      GAUSB     1           Minor axis size of CLEAN beam.
C   BPA       GAUSC     1           Position angle (deg) of BMAJ
C   BADDISK   IBAD     10           Bad disk list.
C   Programmer = J. P. Leahy 1989.
C-----------------------------------------------------------------------
      INTEGER   IERR, IRET, USID, VOL(3), SEQ(3), KAP, NEED
      LOGICAL   OLD, T, ONEPLN, NOCON
      REAL      PLNMAX, PLNMIN
      CHARACTER HILINE*72, NAME(3)*12, CLASS(3)*6, REST*4
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA T /.TRUE./
      DATA REST /'REST'/
C-----------------------------------------------------------------------
C                                       Free AP memory
      CALL QRLSE
      NCFILE = 0
      NSCR = 0
      NOCON  = .FALSE.
      FLUX = 0.0
C                                       Get input values.
      CALL GETIN (USID, NAME, VOL, CLASS, SEQ, IRET)
C                                       Check for restart of AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Get map files and create
C                                       output files if required.
      CALL FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IRET)
      IF (IRET.NE.0) GO TO 990

      CLNLST = CLNLIM
      CLNSTR = CLNLIM
C                                       Decide if more than one plane
C                                       in image.
      ONEPLN = .NOT. ((CATBLK(KINAX+2).GT.1) .OR. (CATBLK(KINAX+3).GT.1)
     *   .OR. (CATBLK(KINAX+4).GT.1) .OR. (CATBLK(KINAX+5).GT.1)
     *   .OR. (CATBLK(KINAX+6).GT.1))
C                                       Get BEAM if not input.
      IF (GAUSA.LT.-0.5) THEN
         NOCON = T
         GAUSA = 0.0
         GAUSB = 0.0
         GAUSC = 0.0
      ELSE IF (GAUSA.LE.0.0) THEN
         IF (CATR(KRBMJ).GE.0.0) THEN
            GAUSA = CATR(KRBMJ) * 3600.
            GAUSB = CATR(KRBMN) * 3600.
            GAUSC = CATR(KRBPA)
         ELSE
C                                       No beam
            IRET = 8
            WRITE (MSGTXT,1400)
            GO TO 990
            END IF
         END IF
C                                       Write inputs to history and
C                                       log files.
      CALL RSTHIS (NAME, CLASS, SEQ, VOL, OLD)
C                                      Grid CCs, FFT, weight.
      NEED = 2 * NX * NY + NX + NY
      KAP = 8 * NY + 106 + 5 * CLNSTR
      NEED = MAX (NEED, KAP)
      NEED = NEED / 1024
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = 8 * NY + 106 + 5 * CLNSTR + 4 * NX
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
      CALL RESTOR (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       FFT to map plane.
      CALL APDFFT (IRET)
      CALL QRLSE
      IF (IRET.NE.0) GO TO 990
C                                       Reaquire AP
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         IRET = 8
         MSGTXT = 'UNABLE TO RE-ACQUIRE DESIRED MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Subtract from dirty map.
      CALL ADDMAP (PLNMAX, PLNMIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Determine RESMAX.
      RESMAX = MAX (ABS (PLNMAX), ABS (PLNMIN))

      CALL QRLSE
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
      IF ((GAUSA.GT.0.0) .AND. (GAUSA.LT.1.E-5)) CATBLK(KITYP) = 4
      CALL CATIO ('UPDT', RESVOL, CNORES, CATBLK, REST, BUFF3, IERR)
      IF ((IERR.EQ.0) .OR. (IERR.GE.5)) GO TO 250
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (8)
C                                       Clear  map files
 250  IRET = IERR
      IF ((IERR.NE.0) .AND. (IERR.LT.9)) GO TO 990
C                                       History
      CALL HIOPEN (LUNCL1, RESVOL, CNORES, BUFF1, IERR)
C                                       Plane number
      WRITE (HILINE,1254) TSKNAM, BLC(3)
      IF (BLC(3).GT.1) CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
C                                        Write total restored flux
      WRITE (HILINE,1250) TSKNAM, FLUX
      CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
C                                       Write number of iterations
C                                       actually done.
      WRITE (HILINE,1251) TSKNAM, CLNSTR
      CALL HIADD (LUNCL1, HILINE, BUFF1, IERR)
      CALL HICLOS (LUNCL1, T, BUFF1, IERR)
C                                        Finished.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1200 FORMAT ('ERROR',I3,' UPDATING CLEAN HEADER ')
 1250 FORMAT (A6,' /Total Restored flux density = ',1PE12.4,' Jy')
 1251 FORMAT (A6,' /Number of CLEAN components =',I8)
 1254 FORMAT (A6,' / Clean of plane ',I4)
 1400 FORMAT ('ERROR: CAN''T FIND CLEAN BEAM')
      END
      SUBROUTINE ADDMAP (RMAX, RMIN, IERR)
C-----------------------------------------------------------------------
C   ADDMAP adds the transformed GRID map to the input image prodicing
C   the restored image.
C   The transformed grid file is assumed to be in the WRK file.
C   Input:
C   Output:
C     RMAX    R   Maximum in residual/restored map.
C     RMIN    R   Minimum in residual/restored map.
C-----------------------------------------------------------------------
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, WIN(4),
     *   I, IERR
      INTEGER   IAPMAX, IAPMIN, IAPRES, SIZE, IAPSUM, ONENX
      LOGICAL   MAP, EXCL, WAIT
      REAL      RMAX, RMIN, TMAX, TMIN, YTEMP(3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMAX, YTEMP(1)),      (TMIN, YTEMP(3))
      DATA MAP, EXCL, WAIT /.TRUE., 2*.TRUE./
      DATA IAPMAX, IAPMIN, IAPRES
     *   /   2,      4,      10/
C-----------------------------------------------------------------------
C                                       Initialize extrema.
      RMAX = -1.0E20
      RMIN =  1.0E20
C                                       Setup AP locations.
C                                         2 = MAX    (IAPMAX)
C                                         4 = MIN    (IAPMIN)
C                                        10 = Residual map (IAPRES)
C                                        IAPRES+NX+1 Dirty & Sum(IAPSUM)
      ONENX = NX
      SIZE = ONENX + 1
      IAPSUM = IAPRES + SIZE
C                                       Clear AP area for Residual
      CALL QVCLR (IAPRES, 1, ONENX)
C                                       Open and INIT files.
      CALL ZOPEN (LUNDRT, FIND1, DRTVOL, DRTFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      CALL ZOPEN (LUNWRK, FIND2, WRKVOL, WRKFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      CALL ZOPEN (LUNRES, FIND3, RESVOL, RESFIL, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
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
C                                       Loop
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
C                                       Read work file
         CALL MDISK ('READ', LUNWRK, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1105) IERR,I
            GO TO 990
            END IF
         CALL QWR
C                                       Load input
         CALL QPUT (BUFF1(BIND1), IAPSUM, ONENX, 2)
         CALL QWD
C                                       Load work file
         CALL QWR
         CALL QPUT (BUFF2(BIND2), IAPRES, ONENX, 2)
         CALL QWD
C                                       Add rows.
         CALL QVADD (IAPSUM, 1, IAPRES, 1, IAPSUM, 1, ONENX)
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
      SUBROUTINE APDFFT (IERR)
C-----------------------------------------------------------------------
C   APDFFT calls AP FFT routines PASS1 and PASS2.  If the transform
C   can be done all in the AP this is done; if not then a transpose
C   of the type designed by F. Schwab is done.
C   On the reverse transform the GRID file is transformed to the WORK
C   file. The contents of the GRID file are destroyed.
C   Input:
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
      INTEGER   IERR
C
      INTEGER   JDIR, LUN(3), BO(3), VOL(3)
      LOGICAL   FULL
      REAL      SMAX, SMIN
      CHARACTER FIL(3)*48
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA FULL /.FALSE./
C-----------------------------------------------------------------------
C                                       COMPLEX TO REAL
C                                       Set file information.
C                                       1 = Grid, 2 = Resid, 3 = Work
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
      JDIR = -1
C                                       Do disk based FFT.
      CALL PASS1 (JDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1, BUFF2,
     *   BUFSZ2, NX, NY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PASS2 (JDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1, BUFF2,
     *   BUFSZ2, NX, NY, SMAX, SMIN, IERR)
C
 999  RETURN
      END
      SUBROUTINE RSTHIS (NAME, CLASS, SEQ, VOL, OLD)
C-----------------------------------------------------------------------
C   RSTHIS copies the input map history if the output map does not
C   already have a history file.  Then the inputs to RSTOR with
C   the default values are added to the history file.
C   Inputs:
C     NAME(3)    C*12 Names of maps
C                      1 = Inputmap
C                      2 = CC map
C                      3 = Restored map
C     CLASS(3)    C*6  Classes of the map.
C     SEQ(3)      I    Sequence numbers of the maps.
C     VOL(3)      I    Original disk numbers
C     OLD         L    T => Clean map pre-exists & restart > 0
C   Input from common:
C     CNODRT      I    CNO of dirty image
C     CNORES      I    CNO of restored image
C     DRTVOL      I    VOL of dirty image
C     RESVOL      I    VOL of restored image
C   From common /MAPHDR/
C     CATBLK(256) I    Catalog header block for the CLEAN map.
C   From COMMON /CLN/
C     NX          I    Number of cell in the map in RA.
C     NY          I    Number of cells in the map in dec.
C     BLC(7)      I    Specifies plane.
C     CLNLIM      I    number of components restored.(NITER)
C     GAUSA       R    Major axis (FWHM) of CLEAN restoring beam
C     GAUSB       R    Minor axis (FWHM) of CLEAN restoring beam
C     GAUSC       R    Position angle of CLEAN restoring beam.
C     BUFF1(*)    R    Work buffer
C     BUFF2(*)    R    Work buffer
C     LUNCL1      I    LUN for CLEAN history file
C     LUNDRT      I    LUN for dirty history file
C     CLNVER      I    CLEAN comp. file version number.
C   OUTPUT:
C     The input parameters are written on the history file and the
C     log file.
C-----------------------------------------------------------------------
      CHARACTER NAME(3)*12, CLASS(3)*6, XCLS*8, XNAM*12, HILINE*72
      INTEGER   SEQ(3), VOL(3), IERR
      LOGICAL   T, OLD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Open old file: no copy old HI
      IF (OLD) THEN
         CALL HIOPEN (LUNCL1, RESVOL, CNORES, BUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 60
            END IF
         CALL ZDATE (BUFF1(1))
         CALL ZTIME (BUFF1(4))
         CALL TIMDAT (BUFF1(4), BUFF1(1), XCLS, XNAM)
         WRITE (HILINE,1000) TSKNAM, RLSNAM, XNAM, XCLS
         CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
      ELSE
C                                       Copy/open history files.
         CALL HISCOP (LUNDRT, LUNCL1, DRTVOL, RESVOL, CNODRT, CNORES,
     *      CATBLK, BUFF1, BUFF2, IERR)
C                                       Check if old file doesn't exist
         IF (IERR.GT.2) THEN
            IF (IERR.EQ.3) GO TO 50
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (6)
            GO TO 60
            END IF
         END IF
C                                       Add to history file.
C                                       Input map name.
      WRITE (MSGTXT,2000) TSKNAM, NAME(1), CLASS(1), SEQ(1), DRTVOL
      CALL MSGWRT (3)
      CALL HENCO1 (TSKNAM, NAME(1), CLASS(1), SEQ(1), DRTVOL,
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       CC map name.
      WRITE (MSGTXT,2001) TSKNAM, NAME(2), CLASS(2), SEQ(2), VOL(2)
      CALL MSGWRT (3)
      CALL HENCO2 (TSKNAM, NAME(2), CLASS(2), SEQ(2), VOL(2),
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Restored map name.
      WRITE (MSGTXT,2002) TSKNAM, NAME(3), CLASS(3), SEQ(3), RESVOL
      CALL MSGWRT (1)
      CALL HENCOO (TSKNAM, NAME(3), CLASS(3), SEQ(3), RESVOL,
     *   LUNCL1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2015) TSKNAM, BLC
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       CC file version number.
      WRITE (HILINE,2012) TSKNAM, CLNVER
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       Maximum number of components.
      WRITE (HILINE,2003) TSKNAM, CLNLIM
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (1)
      IF (IERR.NE.0) GO TO 50
C                                       CLEAN restoring beam
      WRITE (HILINE,2008) TSKNAM, GAUSA, GAUSB, GAUSC
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (1)
      IF (IERR.EQ.0) GO TO 60
C                                       FACTOR
      WRITE (HILINE,2009) TSKNAM, FACTOR
      CALL HIADD (LUNCL1, HILINE, BUFF2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (1)
      IF (IERR.EQ.0) GO TO 60
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
 1010 FORMAT ('UNABLE TO OPEN OLD CLEAN-MAP HISTORY FILE')
 1020 FORMAT ('RSTHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT ('RSTHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT (A6,'/ INPUT MAP =''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2001 FORMAT (A6,'/ CC MAP = ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2002 FORMAT (A6,'/ RESTORED MAP =''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2003 FORMAT (A6,'NITER = ',I8,' /No. components restored.')
 2008 FORMAT (A6,'BMAJ=',F10.5,' BMIN=',F10.5,' BPA=',F8.3,
     *   ' /CLEAN beam')
 2009 FORMAT (A6,'FACTOR=',F8.3,5X,'/ scall CCs by FACTOR')
 2012 FORMAT (A6,'OUTVER = ',I4,' /CLEAN comp. file version no.')
 2015 FORMAT (A6,'BLC =',7I5,' / Plane CLEANed')
      END
      SUBROUTINE FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   FILES creates the output (Restored) map, and copies the CC file
C   from the CC map to the output map. The location of the cataloged
C   "dirty map" file is left in the DRT file.
C   Dirty map and CC map in catalog are marked READ and restored map
C   is marked WRITE.
C   Input:
C     USID     I    User number.
C     NAME(3)  C*12 Names of maps
C                     1 = Dirty map
C                     2 = CC map
C                     3 = Restored map
C     CLASS(3)  C*6   Classes of the maps
C     VOL(3)     I    volumes of the maps.
C   Commons:
C     CATBLK(256) in /MAPHDR/ a preliminary header for the clean map
C        is returned.
C   Output in common:
C     CNODRT      I    CNO of dirty image
C     CNOCLN      I    CNO of CLEAN file
C     CNORES      I    CNO of restored image
C     DRTVOL      I    VOL of dirty image
C     CLNVOL      I    VOL of CLEAN file
C     RESVOL      I    VOL of restored image
C-----------------------------------------------------------------------
      CHARACTER NAME(3)*12, CLASS(3)*6, STAT*4, REST*4, TYPTMP*2
      INTEGER   ISIZE, LSIZE, IEQUIV, OUTVER, LUNDR2, LUNOLD
      INTEGER   USID, VOL(3), SEQ(3), CATOLD(256), OLDIM, OLDNAX(7), I,
     *   IER, IERR, LREC, IDEP(5), NOSCR, NPIX(2), NONOT, IX, IY
      LOGICAL   OLD
      DOUBLE PRECISION OLDCRV(7)
      REAL      XMAX, XMIN, REQUIV
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IEQUIV, REQUIV)
      DATA REST /'REST'/
C-----------------------------------------------------------------------
C                                       Get catalog slot number for
C                                       the dirty map and it's CATBLK.
      CNODRT = 1
      DRTVOL = VOL(1)
      TYPTMP = 'MA'
      CALL CATDIR ('SRCH', DRTVOL, CNODRT, NAME(1), CLASS(1), SEQ(1),
     *   TYPTMP, USID, STAT, BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(1), CLASS(1), SEQ(1), DRTVOL,
     *      USID
         END IF
C                                       Get CATBLK for dirty map.
C                                       Leave file marked READ.
      CALL CATIO ('READ', DRTVOL, CNODRT, CATBLK, 'READ', BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1090) IERR
         END IF
C                                       Mark map READ in /CFILES/
C                                       Put into slot 2 for APCLN
C                                       compatibility
      NCFILE = 2
      FVOL(NCFILE) = DRTVOL
      FCNO(NCFILE) = CNODRT
      FRW(NCFILE) = 0
C                                       Get dirty scale,offset,max,min.
C                                       Set info into COMMON.
      XMAX = CATR(KRDMX)
      XMIN = CATR(KRDMN)
      RESMAX = MAX (ABS(XMAX) , ABS(XMIN))
      CALL ZPHFIL ('MA', DRTVOL, CNODRT, 1, DRTFIL, IER)
      VOL(1) = DRTVOL
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
C                                       Determine NX and NY
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      CALL POWER2 (NX, IX)
      CALL POWER2 (NY, IY)
      IF ((NX.NE.IX) .OR. (NX.LT.32) .OR. (NX.GT.MAXIMG)) GO TO 980
      IF ((NY.NE.IY) .OR. (NY.LT.32) .OR. (NY.GT.MAXIMG)) GO TO 980

C                                       Set up output map
C                                       Put NAME in CATBLK
      CALL CHR2H (12, NAME(3), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASS(3), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQ(3)
C                                       Create output map file.
      RESVOL = VOL(3)
      CALL MCREAT (RESVOL, CNORES, BUFF1, IERR)
      OLD = .FALSE.
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1190) IERR
            GO TO 990
C                                       Existing file: check it out
         ELSE IF ((CNORES.EQ.CNOCLN) .AND. (RESVOL.EQ.CLNVOL)) THEN
C                                       Can't overwrite input map.
            IERR = 8
            WRITE (MSGTXT,1430)
            GO TO 990
         ELSE IF (CATBLK(KINAX+2).LE.1) THEN
C                                       Won't overwrite output map
C                                       unless it's a cube.
            IERR = 8
            WRITE (MSGTXT,1420)
            GO TO 990
         ELSE
            OLD = .TRUE.
            OLDIM = CATBLK(KIDIM)
            DO 205 I = 1,7
              OLDNAX(I) = CATBLK(KINAX+I-1)
              OLDCRV(I) = CATD(KDCRV+I-1)
 205          CONTINUE
            CALL CATIO('READ', RESVOL, CNORES, CATBLK, REST, BUFF3,
     *         IERR)
            IF (OLDIM.NE.CATBLK(KIDIM)) GO TO 215
            DO 210 I = 1, OLDIM
               IF (OLDNAX(I).NE.CATBLK(KINAX+I-1)) GO TO 215
               IF (ABS (OLDCRV(I)-CATD(KDCRV+I-1)) .GT. 0.05 *
     *             ABS (CATR(KRCIC+I-1))) GO TO 215
 210           CONTINUE
C                                       It matches, mark for write.
            CALL CATIO ('WRIT', RESVOL, CNORES, CATBLK, 'WRIT', BUFF3,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1210) IERR
               GO TO 990
               END IF
            END IF
         END IF
      GO TO 220
C                                       Output file wrong shape!
 215  CONTINUE
         IERR = 8
         WRITE (MSGTXT, 1215)
         GO TO 990
 220  CONTINUE
C                                       Actual seq #
      SEQ(3) = CATBLK(KIIMS)
C                                       Add to /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = RESVOL
      FCNO(NCFILE) = CNORES
      FRW(NCFILE) = 2
      VOL(3) = RESVOL
      LREC = 3
C                                       Copy existing tables files
C                                       to output.
      IF (.NOT.OLD) THEN
         LUNDR2 = 29
         NONOT = 0
         TYPTMP = '  '
         CALL ALLTAB (NONOT, TYPTMP, LUNDR2, LUNCL2, DRTVOL, RESVOL,
     *      CNODRT, CNORES, CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1450) IERR
            GO TO 990
            END IF
C                                       copy keywords too
         CALL KEYPCP (DRTVOL, CNODRT, RESVOL, CNORES, 0, ' ', IERR)
         END IF
C                                       Open CC file.
C                                       Get catalog slot for CC map.
      CNOCLN = 1
      CLNVOL = VOL(2)
      TYPTMP = 'MA'
      CALL CATDIR ('SRCH', CLNVOL, CNOCLN, NAME(2), CLASS(2), SEQ(2),
     *   TYPTMP, USID, STAT, BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(1), CLASS(1), SEQ(2), CLNVOL,
     *      USID
         GO TO 990
         END IF
C                                       Copy CATBLK and mark CC map READ
      CALL CATIO ('READ', CLNVOL, CNOCLN, CATOLD, 'READ', BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Update /CFILES/ N.B. For
C                                       compatibility with APCLN
C                                       subroutines, we use slot 1 in
C                                       /CFILES/ for the CC Map.
      NCFILE = 1
      FVOL(NCFILE) = CLNVOL
      FCNO(NCFILE) = CNOCLN
      FRW(NCFILE) = 0
      VOL(2) = CLNVOL
      NCFILE = 3
C                                       Create/get ver # for CC file.
      IF (CLNVER.EQ.0) CALL FNDEXT ('CC', CATOLD, CLNVER)
      IF (CLNVER.EQ.0) THEN
         WRITE (MSGTXT,1410)
         GO TO 990
         END IF
C                                        Copy CC file to output.
      OUTVER = 0
      LUNOLD = 28
C                                       If cube, override OUTVER w/
C                                       plane number.
      IF (CATBLK(KINAX+2).GT.1) OUTVER = BLC(3)
C                                       Get number of highest
C                                       existing CC file
      IF (OUTVER.EQ.0) CALL FNDEXT ('CC', CATBLK, OUTVER)
C                                       Write to next one.
      OUTVER = OUTVER + 1
C                                       Check limit of 46655
      IF (OUTVER.GT.46655) THEN
         IERR = 5
         WRITE (MSGTXT,1301) OUTVER
         GO TO 990
         END IF
      CALL TABCOP ('CC', CLNVER, OUTVER, LUNOLD, LUNCL2, CLNVOL, RESVOL,
     *   CNOCLN, CNORES, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1400)
         GO TO 990
         END IF
C                                       From now on, use CC file
C                                       on output map:
      CLNVER = OUTVER
      CLNVOL = RESVOL
      CNOCLN = CNORES
      CALL CCINI (LUNCL2, LREC, CLNVOL, CNOCLN, CLNVER, CATBLK,
     *   BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1300) IERR
         GO TO 990
         END IF
C                                       Check no. CC comps.
      REQUIV = BUFF3(5)
      IF (CLNLIM.GT.IEQUIV .OR. CLNLIM.EQ.0) CLNLIM = IEQUIV
      IEQUIV = CLNLIM
      BUFF3(5) = REQUIV
C                                       Close.
      CALL TABIO ('CLOS', 0, LREC, BUFF1, BUFF3, IERR)
C                                       Create scratch files.
C                                       Use output as residual file
C                                       Grid file
      NSCR = 0
      NPIX(1) = NX
      NPIX(2) = NY + 1
      CALL MAPSIZ (2, NPIX, ISIZE)
      NPIX(1) = 2 * NY
      NPIX(2) = NX/2 + 1
      CALL MAPSIZ (2, NPIX, LSIZE)
      IF (LSIZE.GT.ISIZE) ISIZE = LSIZE
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.EQ.0) GO TO 25
         WRITE (MSGTXT,1020) IERR
         GO TO 990
C                                       Update from COMMON /FILES/
 25   GRDVOL = SCRVOL(NSCR)
      CNOGRD = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', GRDVOL, CNOGRD, 1, GRDFIL, IERR)
      NOSCR = NSCR
C                                       Work file
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1025) IERR
         GO TO 990
C                                       Update from COMMON /FILES/
 30   WRKVOL = SCRVOL(NSCR)
      CNOWRK = SCRCNO(NSCR)
      CALL ZPHFIL ('SC', WRKVOL, CNOWRK, 1, WRKFIL, IERR)
C                                       Use output file as resid. file
      CALL ZPHFIL ('MA', RESVOL, CNORES, 1, RESFIL, IERR)
      BORES = BODRT

      GO TO 999
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
 1090 FORMAT ('FILES: CANNOT COPY MAP CATBLK, ERROR',I3)
 1190 FORMAT ('FILES: COULD NOT CREATE OUTPUT MAP FILE, ERROR ',I3)
 1210 FORMAT ('FILES: CANNOT UPDATE OUTPUT CATBLK, ERROR',I3)
 1215 FORMAT ('OLD OUTPUT MAP NOT COMPATIBLE WITH INPUT MAP')
 1300 FORMAT ('FILES: UNABLE TO CREATE CLEAN COMP. FILE, ERROR',I3)
 1301 FORMAT ('COMPONENT FILE NO. TOO LARGE ',I3,' > 46655')
 1400 FORMAT ('FILES: CAN''T COPY CC FILE TO OUTPUT')
 1410 FORMAT ('FILES: CAN''T FIND CLEAN COMPONENT FILE')
 1420 FORMAT ('FILES: WON''T OVERWRITE EXISTING SINGLE-PLANE MAP')
 1430 FORMAT ('FILES: CAN''T OVERWRITE INPUT MAP')
 1450 FORMAT ('FILES: ERROR', I3, ' COPYING EXTENSION FILES')
 1980 FORMAT ('MAP DIMENSIONS:',2I5,' NOT INTEGER POWERS OF 2')
      END
      SUBROUTINE GETIN (USID, NAME, VOL, CLASS, SEQ, IERR)
C-----------------------------------------------------------------------
C   GETIN gets the input parameters for the program from AIPS
C  See Prologue for RSTOR for more details.
C-----------------------------------------------------------------------
      CHARACTER TYPTMP*2
      CHARACTER PRGNAM*6, NAME(3)*12, CLASS(3)*6, DEFCLS*6, BLANK*12,
     *   QM*4, UM*4, VM*4, RM*4, LM*4, ICLN*4, QCLN*4, UCLN*4,
     *   VCLN*4, RCLN*4, LCLN*4
      HOLLERITH XNAM1(3), XCLAS1(2), XNAM2(3), XCLAS2(2), XNAM3(3),
     *   XCLAS3(2)
      INTEGER   USID, SEQ(3), VOL(3), NPARMS, IERR, IND, I, IROUND,
     *   ITEMP
      REAL   XSEQ1, XSEQ2, XSEQ3, XVOL1, XVOL2, XVOL3, XBLC(7),
     *   XNI, XBMJ, XBMN, XBPA, XBAD(10), XVER, XFACT
      LOGICAL   T, F, EQUAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XVOL1, XBLC, XNAM2, XCLAS2,
     *   XSEQ2, XVOL2, XVER, XNAM3, XCLAS3, XSEQ3, XVOL3,
     *   XNI, XBMJ, XBMN, XBPA, XFACT, XBAD
      DATA BLANK /' '/
      DATA PRGNAM /'RSTOR '/
      DATA QM,    UM,    VM,    RM,    LM
     *   /'QMAP','UMAP','VMAP','RMAP','LMAP'/
      DATA ICLN,  QCLN,  UCLN,  VCLN,  RCLN,  LCLN
     *   /'IRST','QRST','URST','VRST','RRST','LRST'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize common parameters.
C                                        global areas
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL HIINIT (3)
C                                        RSTOR common areas
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
      CLNLIM = 0
      CLNSTR = 0
      CLNLST = 0
      FLUX = 0
      NUMBIN = 1024
      ITEMP = 2  * MABFSL
      BUFSZ1 = ITEMP
      BUFSZ2 = ITEMP
      BUFSZ3 = ITEMP
      BPS = NBPS
C                                       Get AIPS adverbs.
      NPARMS = 44
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Convert names
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
      CNODRT = 1
      TYPTMP = 'MA'
      CALL MAPOPN ('READ', VOL, NAME(1), CLASS(1), SEQ, TYPTMP, USID,
     *   LUNBEM, IND, CNODRT, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL MAPCLS ('READ', VOL, CNODRT, LUNBEM, IND, CATBLK, F, BUFF1,
     *   IERR)
C                                       Default name for Restored and
C                                       CC maps is Dirty map name.
      EQUAL = NAME(2) .EQ. BLANK(1:12)
      IF (EQUAL) NAME(2) = NAME(1)
C                                       Copy CLASSes or default.
      EQUAL = BLANK(1:6) .EQ. CLASS(2)
      IF (EQUAL) CLASS(2) = CLASS(1)
C                                       Use appropriate default for
C                                       polarization type.
      DEFCLS = '      '
      DEFCLS(1:4) = ICLN(1:4)
      EQUAL = QM(:1) .EQ. CLASS(1)(:1)
      IF (EQUAL) DEFCLS(1:4) = QCLN(1:4)
      EQUAL = UM(:1) .EQ. CLASS(1)(:1)
      IF (EQUAL) DEFCLS(1:4) = UCLN(1:4)
      EQUAL = VM(:1) .EQ. CLASS(1)(:1)
      IF (EQUAL) DEFCLS(1:4) = VCLN(1:4)
      EQUAL = RM(:1) .EQ. CLASS(1)(:1)
      IF (EQUAL) DEFCLS(1:4) = RCLN(1:4)
      EQUAL = LM(:1) .EQ. CLASS(1)(:1)
      IF (EQUAL) DEFCLS(1:4) = LCLN(1:4)
C                                       Get volume numbers
      VOL(2) = IROUND (XVOL2)
      VOL(3) = IROUND (XVOL3)
C                                       Get sequence numbers
      SEQ(2) = IROUND (XSEQ2)
      SEQ(3) = IROUND (XSEQ3)
      IF (SEQ(2).LE.0) SEQ(2) = SEQ(1)
      CALL MAKOUT (NAME, CLASS, SEQ, DEFCLS, NAME(3), CLASS(3),
     *   SEQ(3))
C                                       Get CC file version no.
      CLNVER = IROUND (XVER)
C                                       Get Restoring information
      CLNLIM = XNI + 0.5
      GAUSA = XBMJ
      GAUSB = XBMN
      GAUSC = XBPA
C                                       Check number of comps
C                                       subtracted from dirty map.
      PRECLN = 0
      IF (CATBLK(KINIT).GT.0) PRECLN = CATBLK(KINIT)
C                                       factor
      FACTOR = XFACT
      IF (FACTOR.EQ.0.0) FACTOR = 1.0
      IF (ABS(FACTOR).NE.1.0) THEN
         WRITE (MSGTXT,1020) FACTOR
         CALL MSGWRT (7)
         END IF
C                                       Get bad disks.
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 70      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('WARNING: FACTOR=',F7.2,' NOT 1 OR -1')
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
C     Transform of CLEAN components times the restoring fn added to the
C     file GRDFIL.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   FIND1, FIND2, BIND2, BIND3, WIN(4), NLOAD, I, JLIM
      INTEGER   FIRST, NUMCLN, MAXCMP, JNUM, IAPSIZ,
     *   WRK1, WRK2, NAPGAU, NAPEX1, NAPEXP, NAPRES, NAPGRD,
     *   NAPCMP, NAPCMS, NUMBER, APBUF, ONENY, TWONY
      LOGICAL   MAP, EXCL, WAIT, T, F, GETOLD
      REAL      TEMP, GBUFF(1), OBUFF(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (OBUFF(1), BUFF1(1)),      (GBUFF(1), BUFF2(1))
      DATA APBUF / 1 /
      DATA EXCL, WAIT, MAP, T, F /4*.TRUE., .FALSE./
C-----------------------------------------------------------------------
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
C                                       Set size of AP
      IAPSIZ = PSAPNW * 1024
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
      GETOLD = F
C                                       Begin component loop.
 10   CONTINUE
         NUMBER = MAXCMP
         IF (NUMCLN.LT.MAXCMP) NUMBER = NUMCLN
C                                       Load CLEAN components this pass.
         FIRST = CLNLST
         JNUM = NUMBER
         CALL CMPCRM (NAPCMS, APBUF, FIRST, NUMBER, NLOAD, T, IERR)
         IF (IERR.NE.0) GO TO 999
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NY * 2
         WIN(4) = NX / 2 + 1
         IF (GETOLD) THEN
C                                       Open and INIT GRID file for read
            CALL ZOPEN (LUNGD1, FIND2, GRDVOL, GRDFIL, MAP, EXCL, WAIT,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 990
               END IF
            CALL MINIT ('READ', LUNGD1, FIND2, WIN(3), WIN(4), WIN,
     *         GBUFF, BUFSZ2, BOGRD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1050) IERR
               GO TO 990
               END IF
            END IF
C                                       Open and INIT WRK file for writ
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
            IF (GETOLD) THEN
               CALL MDISK ('READ', LUNGD1, FIND2, GBUFF, BIND2, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1080) IERR,I
                  GO TO 990
                  END IF
C                                       Load row from GRID file.
               CALL QPUT (GBUFF(BIND2), NAPGRD, TWONY, 2)
               CALL QWD
            ELSE
               CALL QVCLR (NAPGRD, 1, TWONY)
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
C                                       Multiply by restoring function.
            CALL QCRVMU (NAPCMP, 2, NAPRES, 1, NAPCMP, 2, ONENY)
            CALL QWR
C                                       Add to GRID file.
            IF (GETOLD) CALL QVADD (NAPCMP, 1, NAPGRD, 1, NAPCMP, 1,
     *         TWONY)
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
C                                       Finish writing WRK file.
         CALL MDISK ('FINI', LUNWRK, FIND1, OBUFF, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR, JLIM
            GO TO 990
            END IF
C                                       End of this pass, close files.
         CALL ZCLOSE (LUNWRK, FIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1160) IERR
            CALL MSGWRT (8)
            END IF
         IF (GETOLD) THEN
            CALL ZCLOSE (LUNGD1, FIND2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1160) IERR
               CALL MSGWRT (8)
               END IF
            END IF
         CLNLST = CLNLST + JNUM
         NUMCLN = NUMCLN - JNUM
C                                       Switch WRK and GRID files.
         CALL FSWTCH (GRDFIL, WRKFIL, GRDVOL, WRKVOL, CNOGRD, CNOWRK,
     *      BOGRD, BOWRK)
C                                       Roll AP if necessary
         IF (CLNLST.LE.CLNSTR) CALL QROLL (NAPCMS, BUFF3, BUFSZ3, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Check for more CLEAN components.
         GETOLD = T
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
      INTEGER   IBUFF(512), IERR, NREC, NLOAD, LIMIT, NCOUNT, I, LREC,
     *   NKEY, KOLS(4), RAKOL, DECKOL, FLXKOL, TYPKOL, APLOC, APLOC1,
     *   APLOC2, APLOC3, APLOC4, APBUF, APLO, NNCNT, JJCNT, IPOINT,
     *   ICOUNT, JCOUNT
      REAL      CFLUX(1), TEMP(6), TWOPIX, RAC(1), DECC(1),
     *   RECORD(7), XCEN, YCEN
      LOGICAL   LFIRST, FA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RSTOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL)
      EQUIVALENCE (BUFF1(1), IBUFF(1))
      EQUIVALENCE (BUFF2(1), CFLUX(1)),      (BUFF3(1), RAC(1)),
     *   (BUFF3(MABFSL/2+1), DECC(1))
      DATA KEYS /'DELTAX  ', 'DELTAY  ', 'FLUX    ', 'TYPE OBJ'/
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
      CALL TABINI ('READ', 'CC', CLNVOL, CNOCLN, CLNVER, CATBLK,
     *   LUNCL2, NKEY, NREC, LREC, BUFF2, IBUFF, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         GO TO 990
 10   CALL RFILL (7, 0.0, RECORD)
      NKEY = 3
      TYPKOL = 7
      FLXKOL = 1
      RAKOL = 2
      DECKOL = 3
      IF (LREC.GT.3) NKEY = 4
      CALL FNDCOL (NKEY, KEYS, 8, FA, IBUFF, KOLS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
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
            IF (IERR.LE.0) GO TO 40
               WRITE (MSGTXT,1030) IERR, IPOINT
               GO TO 990
 40         IF ((LREC.GT.3) .AND. (RECORD(TYPKOL).NE.0.0)) GO TO 45
            IF (IERR.LT.0) GO TO 45
               NCOUNT = NCOUNT + 1
               CFLUX(NCOUNT) = RECORD(FLXKOL) * FACTOR
               RAC(NCOUNT) = RECORD(RAKOL) + XCEN
               DECC(NCOUNT) = RECORD(DECKOL) + YCEN
 45         IPOINT = IPOINT + 1
 50         CONTINUE
         IF (NCOUNT.LE.0) GO TO 55
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
 55      ICOUNT = ICOUNT + LIMIT
         JCOUNT = JCOUNT + NCOUNT
C                                       If load complete close CLNFIL
C                                       and return.
         IF (ICOUNT.GE.NUMBER) GO TO 60
C                                       Update APLOC.
            APLOC = APLOC + NNCNT * 5
            IF (NUMBER-ICOUNT.LT.LIMIT) LIMIT = NUMBER - ICOUNT
C                                       Return for another load.
            GO TO 30
C                                       IF (LFIRST) sum fluxes.
 60   IF (.NOT.LFIRST) GO TO 65
         APLOC1 = APLO + 1
         JJCNT = JCOUNT
         CALL QSVE (APLOC1, 5, 1, JJCNT)
         CALL QWR
         CALL QGET (TEMP, 1, 1, 2)
         CALL QWD
         FLUX = FLUX + TEMP(1)
C                                     Make sure CC loaded.
 65   IF (JCOUNT.GT.0) GO TO 70
         WRITE (MSGTXT,1065)
         IERR = 1
         GO TO 990
 70   NUMBER = JCOUNT
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
      INTEGER   I, JLIM, K, NAPRES, NAPGAU, NAPEXP, NAPEX1, NAPGRD,
     *   NAPCMP, NAPCMS, NAPRS1, ONENY, TWONY, WRK1, WRK2
      INCLUDE 'RSTOR.INC'
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
