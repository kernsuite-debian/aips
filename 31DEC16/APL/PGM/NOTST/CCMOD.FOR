      PROGRAM CCMOD
C-----------------------------------------------------------------------
C! Adds components to a CC table to model a Gaussian or disk.
C# Modeling EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998
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
C   CCMOD inserts a group of clean components into an existing CC
C   extension file to model a source as an elliptical uniformly bright
C   disk or Gaussian.
C   VERSION FOR CC TABLES FILES
C   Inputs:
C       INNAME(3)   R     Image name (name)
C       INCLASS(2)  R     Image name (class)
C       INSEQ       R     Image name (seq #)
C       INDISK      R     Input disk unit #
C       INVER       I     CC file ver. #
C       OPCODE      C*4   Model type; 'DISK' = '    ', or 'GAUS'
C       FLUX        R     Source flux (map units*square pixel)
C       BMAJ        R     Source major axis (pixels)
C       BMIN        R     Source minor axis (pixels)
C       BPA         R     Source position angle of major axis
C       MAPXY       R     Source center position (pixels)
C       BITER       R     If >0 start replacing components after
C                         this component
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*12, CLASIN*6, MTYPE*2, STAT*4, PNAM*6,
     *   TYPNAM(5)*4, TYPE*4
      INTEGER   CATBLK(256), NPARMS, PORK(768), IRET, IERR, VER, NCOL,
     *   CATNO, IROUND, IDISK, LUN, SEQIN, NREC, X1, X2, Y1, Y2, IX, IY,
     *   IPTR(256), ITYP, NKEY, NEW, NEXT, NCOMP
      REAL      A, B, C, S, CO, DX, DY, XVER, PI, RFAC(2), XM, BRIGHT,
     *   CLSTRT, XS, YS, C4(128), DISKIN, SEQ, BMAJ, BMIN, BPA, MAPX,
     *   MAPY, MAPZ(5), FLUX, PIG(512), RECORD(20), SFACT, SUM, RADIUS,
     *   ADD
      LOGICAL   RQUICK, LCLAS, F, T
      HOLLERITH XNAMEI(3), XCLASI(2), XTYPE
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /INPARM/ XNAMEI, XCLASI, SEQ, DISKIN, XVER, XTYPE, FLUX,
     *   BMAJ, BMIN, BPA, MAPX, MAPY, MAPZ, CLSTRT
      EQUIVALENCE (PIG, PORK)
      EQUIVALENCE (C4, CATBLK)
      DATA NPARMS, PNAM /21, 'CCMOD '/
      DATA F, T /.FALSE.,.TRUE./
      DATA TYPNAM /'DISK','GAUS','POIN', 2*'    '/
      DATA PI /3.14159/,            RFAC /0.5, 1.6 /
C-----------------------------------------------------------------------
      CALL RFILL (20, 0.0, RECORD)
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NEW = 0
      CALL GTPARM (PNAM, NPARMS, RQUICK, XNAMEI, PORK, IERR)
      IF (IERR.EQ.0) GO TO 10
         IRET = 1
         RQUICK = F
         IF (IERR.EQ.1) GO TO 900
         WRITE (MSGTXT,1000)
         GO TO 990
 10   IRET = 0
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (4, 1, XTYPE, TYPE)
      IF (RQUICK) CALL RELPOP (IRET, PORK, IERR)
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
      SEQIN = IROUND (SEQ)
      IDISK = IROUND (DISKIN)
      VER = IROUND (XVER)
      DO 40 ITYP = 1,5
         LCLAS = TYPE(1:4) .EQ. TYPNAM(ITYP)(1:4)
         IF (LCLAS) GO TO 50
 40      CONTINUE
 50   IF (ITYP.GT.3) ITYP = 1
      NEXT = CLSTRT + 0.01
      IF (NEXT.LT.0) NEXT = 0
C                                       Locate map in directory.
      CATNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', IDISK, CATNO, NAMEIN, CLASIN, SEQIN, MTYPE,
     *   NLUSER, STAT, PORK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'REQUESTED MAP NOT FOUND IN CATALOG DIRECTORY'
         GO TO 990
         END IF
      WRITE (MSGTXT,1020) NAMEIN, CLASIN, SEQIN, IDISK, CATNO
      CALL MSGWRT (3)
C                                       Read catalog block, get # comps
      CALL CATIO ('READ', IDISK, CATNO, CATBLK, 'WRIT', PORK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         GO TO 990
         END IF
C                            Calculate center positions
      XS = C4(KRCRP)
      YS = C4(KRCRP+1)
C                                       Open clean comp. file
      NREC = 1000
      NCOL = 3
      LUN = 27
      NKEY = 0
C                                       default 0 => high current
      IF (VER.EQ.0) CALL FNDEXT ('CC', CATBLK, VER)
      IF (VER.EQ.-1) THEN
         CALL FILL (256, 0, IPTR)
         CALL FILL (7, 12, IPTR(129))
         NEXT = 0
         END IF
      CALL CCINI (LUN, NCOL, IDISK, CATNO, VER, CATBLK, PORK, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1006) IERR
         GO TO 980
         END IF
      NCOMP = PORK(5)
      IF ((NEXT.GT.NCOMP) .OR. (NEXT.LE.0)) NEXT = NCOMP
C                                       calculate some elliptical nos.
      IF (ITYP.EQ.3) GO TO 400
         BRIGHT = 4 * FLUX / (PI * BMAJ * BMIN)
         BMAJ = BMAJ * RFAC(ITYP)
         BMIN = BMIN * RFAC(ITYP)
         S    = SIN (BPA*PI/180.)
         CO   = COS (BPA*PI/180.)
         A = (CO/BMAJ)**2 + (S/BMIN)**2
         B = (S/BMAJ)**2 + (CO/BMIN)**2
         C = 2. * CO * S * ((1./BMIN)**2 - (1./BMAJ)**2)
C                                       maximum extent in Y
         DY = SQRT ((S*BMIN)**2 + (CO*BMAJ)**2)
         Y1 = 0.5 + MAPY - DY
         Y2 = MAPY + DY + 0.5
         SUM = 0.0
C                                       loop over all points within elli
         DO 300 IY = Y1,Y2
            DY = IY - MAPY
            DX = A + (C*C - A*B) * DY**2
            IF (DX.GE.0.0) THEN
               XM = -C * DY/A
               DX = SQRT(DX) / A
               X1 = 0.5 + XM + MAPX - DX
               X2 = XM + MAPX + DX + 0.5
               DO 200 IX = X1,X2
                  RECORD(2) = (IX-XS) * C4(KRCIC)
                  RECORD(3) = (IY-YS) * C4(KRCIC+1)
                  DX = IX - MAPX
                  RADIUS = A * DY ** 2 + B * DX ** 2 + C * DY * DX
                  ADD = BRIGHT * SFACT (ITYP, RADIUS)
                  IF (ADD.NE.0.0) THEN
                     RECORD(1) = ADD
                     SUM = SUM + ADD
                     NEXT = NEXT + 1
                     NEW = NEW + 1
                     CALL TABIO ('WRIT', 0, NEXT, RECORD, PORK, IERR)
                     IF (IERR.NE.0) GO TO 975
                     END IF
 200              CONTINUE
               END IF
 300        CONTINUE
         GO TO 500
C                                       Point source
 400  CONTINUE
         RECORD(1) = FLUX
         RECORD(2) = (MAPX-XS) * C4(KRCIC)
         RECORD(3) = (MAPY-YS) * C4(KRCIC+1)
         NEXT = NEXT + 1
         NEW = NEW + 1
         SUM = SUM + FLUX
         CALL TABIO ('WRIT', 0, NEXT, RECORD, PORK, IERR)
         IF (IERR.NE.0) GO TO 975
C                                       Close it up
 500  CALL TABIO ('CLOS', 0, NEXT, RECORD, PORK, IERR)
      IF (IERR.NE.0) GO TO 975
      CATBLK(KINIT) = NEXT
      WRITE (MSGTXT,1200) NEW, SUM
      CALL MSGWRT (4)
      CALL CATIO ('UPDT', IDISK, CATNO, CATBLK, 'CLWR', PORK, IERR)
      GO TO 995
C                                       error messages
 900  WRITE (MSGTXT,1001)
      CALL MSGWRT (1)
      GO TO 999
C
 975  WRITE (MSGTXT,1008) IERR
 980  CALL MSGWRT (8)
      CALL CATIO ('UPDT', IDISK, CATNO, CATBLK, 'CLWR', PORK, IERR)
      GO TO 995
C
 990  CALL MSGWRT (8)
C                                   Close files and go to bed.
 995  CALL DIETSK (IRET, RQUICK, PORK)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('DISK PROBLEMS, GTPARM')
 1001 FORMAT ('CAN''T FIND INITIATOR, GTPARM')
 1005 FORMAT ('CATIO ERROR NO',I5)
 1006 FORMAT ('CAN''T OPEN CCFILE,  ERRNO',I5)
 1008 FORMAT ('ERROR NO',I5,' IN WRITING CCFILE WITH TABIO')
 1020 FORMAT ('Found ',A12,A6,' Seq',I4,' Disk:',I3,' in slot no',I5)
 1200 FORMAT ('Added ',I6,' comps to file:flux=',E12.3)
 4000 FORMAT ('You are using a non-standard program')
      END
      REAL FUNCTION SFACT (ITYPE, R)
C-----------------------------------------------------------------------
C   Determine the intensity of a point component at a given normalized
C   distance from source center so that the sum of the point components
C   is 1.0; this for various model types.
C   Inputs:
C      ITYPE    I      Model type: 1 => disk; 2 => gaussian
C      R        R      Normalized radius.  For disks this
C                      is the radius; for gauss 1.6 * FWHP dize
C-----------------------------------------------------------------------
      INTEGER   ITYPE
      REAL      R, C1, C2
C                              (3.2**2) * Ln (2.0), Ln (2.0)
      DATA C1/7.097827 /, C2 /.6931472  /
C-----------------------------------------------------------------------
      IF (ITYPE.NE.2) THEN
         SFACT = 1.0
         IF (R.GT.1.0) SFACT = 0.0
      ELSE
         SFACT = C2 * EXP (-C1 * R)
         END IF
C
 999  RETURN
      END
