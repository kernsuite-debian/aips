      SUBROUTINE SETDO3 (UVDISK, UVCNO, IBUFF, IRET)
C-----------------------------------------------------------------------
C! Sets up for UV model computation filling DO3DIM parameter
C# UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2000, 2005, 2008-2009, 2012
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
C   SETDO3 uses some parameters in common /MAPDES/ to find and read the
C   headers of Model images.  It uses this to determine whether 3D
C   imaging or parallel planes images are present.
C   Inputs:
C      UVDISK   I      Disk number for uv data set
C                         O -> scratch file
C      UVCNO    I      Catalog number for UV data set
C                         If scratch, scrcno(uvcno)
C   Inputs from DGDS.INC:
C      CCDISK   I(*)   Disk numbers of files
C      CCCNO    I(*)   Catalog numbers of files.
C      MFIELD   I      Number of fields.
C   Output:
C      IBUFF    I(256) Work buffer
C      IRET     I      Return code, 0=>OK, otherwise failed.
C   Output in DGDS.INC
C      DO3DIM   L      Using 3D imaging?
C   UVPGET must be called before calling this routine too.
C-----------------------------------------------------------------------
      INTEGER   UVDISK, UVCNO, IBUFF(*), IRET
C
      INTEGER   I, CATCLN(256), NCEN, NOFF, ONCE, ONCED, ONCEC, IRAOFF,
     *   IDECOF, LOCS(2), VALUES(2), KEYTYP(2), IERR, J, K, DISKUV,
     *   CNOUV
      REAL      CATCR(256), MAXBLN, PMAT(3,3), UMATN(3,3)
      DOUBLE PRECISION CATCD(128), CLNRA, CLNDEC
      HOLLERITH CATCH(256)
      LOGICAL   DOUMAT, WASDIF, IN3D, FIRST
      CHARACTER CHTEMP*8
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATCLN, CATCH, CATCR, CATCD)
      SAVE ONCE, ONCED, ONCEC
      DATA ONCE, ONCED, ONCEC /-1, -1, -1/
C-----------------------------------------------------------------------
      NCEN = 0
      NOFF = 0
      WASDIF = .FALSE.
      FIRST = .TRUE.
      IN3D = DO3DIM
      IF (UVDISK.GT.0) THEN
         DISKUV = UVDISK
         CNOUV = UVCNO
      ELSE
         DISKUV = SCRVOL(UVCNO)
         CNOUV = SCRCNO(UVCNO)
         END IF
      DOUMAT = (DISKUV.NE.ONCED) .OR. (CNOUV.NE.ONCEC)
      ONCED = DISKUV
      ONCEC = CNOUV
C                                       header of main UV: get rot
      IF (DOUMAT) THEN
         CALL CATIO ('READ', DISKUV, CNOUV, CATCLN, 'REST', IBUFF, IRET)
         IF (IRET.NE.0) THEN

            WRITE (MSGTXT,1010) IRET, DISKUV, CNOUV
            GO TO 990
            END IF
         CALL ROTFND (CATCR, UVROT, IRET)
         END IF
C                                       Loop thru other fields.
 25   DO 40 I = 1,MFIELD
C                                       Read catalog block
         CALL CATIO ('READ', CCDISK(I), CCCNO(I), CATCLN, 'REST',
     *      IBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, CCDISK(I), CCCNO(I)
            GO TO 990
            END IF
         IF (FLDSZ(1,I).LE.0) FLDSZ(1,I) = CATCLN(KINAX)
         IF (FLDSZ(2,I).LE.0) FLDSZ(2,I) = CATCLN(KINAX+1)
C                                       3D image?
         CHTEMP = 'RA'
         CALL AXEFND (2, CHTEMP, KICTPN, CATCH(KHCTP), IRAOFF, IERR)
         CHTEMP = 'DEC'
         CALL AXEFND (3, CHTEMP, KICTPN, CATCH(KHCTP), IDECOF, IERR)
         IF (FIRST) THEN
            IF (((ABS(CATCR(KRCRP+IRAOFF)-CATCLN(KINAX+IRAOFF)/2.0).LT.
     *         0.01) .AND. (ABS(CATCR(KRCRP+IDECOF)-1.0-
     *         CATCLN(KINAX+IDECOF)/2.0).LT.0.01)) .OR.
     *         (CATCLN(KIITY).EQ.2)) NCEN = NCEN + 1
            IF ((ABS(CATCD(KDCRV+IRAOFF)-RA).GT.
     *         0.01*ABS(CATCR(KRCIC+IRAOFF))) .OR.
     *         (ABS(CATCD(KDCRV+IDECOF)-DEC).GT.
     *         0.01*ABS(CATCR(KRCIC+IDECOF)))) NOFF = NOFF + 1
               END IF
C                                       rotation matrix in case
         IF (DOUMAT) THEN
            IF (.NOT.WASDIF) CALL RCOPY (9, UMATS(1,1,I), UMATN)
            CLNRA = CATCD(KDCRV+IRAOFF)
            CLNDEC = CATCD(KDCRV+IDECOF)
            CALL ROTFND (CATCR, MAPROT, IRET)
            IF (DO3DIM) THEN
               CALL PRJMAT (RA, DEC, UVROT, CLNRA, CLNDEC, MAPROT,
     *            UMATS(1,1,I), PMAT)
            ELSE
               CALL P2DMAT (RA, DEC, UVROT, CLNRA, CLNDEC, MAPROT,
     *            UMATS(1,1,I), PMAT)
               END IF
            IF (.NOT.WASDIF) THEN
               DO 35 K = 1,3
                  DO 30 J = 1,3
                     IF (ABS(UMATS(J,K,I)-UMATN(J,K)).GT.3.0E-6)
     *                  WASDIF = .TRUE.
 30                  CONTINUE
 35               CONTINUE
               END IF
            END IF
 40      CONTINUE
C                                       Check/set 3D
      IF (FIRST) THEN
         DO3DIM = .FALSE.
         IF (NOFF.EQ.0) THEN
            MSGTXT = 'SETDO3: imaging done with one tangent plane'
            IF ((MFIELD.GT.1) .AND. (ONCE.NE.1)) THEN
               CALL MSGWRT (2)
               MSGTXT = 'SETDO3: WARNING, THIS HAS CHANGED'
               IF (ONCE.NE.-1) CALL MSGWRT (6)
               END IF
            ONCE = 1
         ELSE IF (NCEN.EQ.MFIELD) THEN
            DO3DIM = .TRUE.
            MSGTXT = 'SETDO3: imaging done with multiple tangent planes'
            IF (ONCE.NE.2) THEN
               CALL MSGWRT (4)
               MSGTXT = 'SETDO3: WARNING, THIS HAS CHANGED'
               IF (ONCE.NE.-1) CALL MSGWRT (6)
               END IF
            ONCE = 2
         ELSE IF (NCEN.EQ.0) THEN
            MSGTXT = 'SETDO3: imaging done with one OFFSET' //
     *         ' tangent plane'
            IF (ONCE.NE.3) THEN
               CALL MSGWRT (2)
               MSGTXT = 'SETDO3: WARNING, THIS HAS CHANGED'
               IF (ONCE.NE.-1) CALL MSGWRT (6)
               END IF
            ONCE = 3
         ELSE
            WRITE (MSGTXT,1050) NCEN, NOFF, MFIELD
            CALL MSGWRT (7)
            ONCE = 0
            END IF
         IF ((DO3DIM.NEQV.IN3D) .AND. (DOUMAT)) THEN
            FIRST = .FALSE.
            GO TO 25
            END IF
         END IF
C                                       Header keyword
      IF ((DOUMAT) .AND. (WASDIF)) THEN
         CALL CATKEY ('REED', DISKUV, CNOUV, 'MAXABSU ', 1, LOCS,
     *      VALUES, KEYTYP, IBUFF, I)
         IF ((I.EQ.0) .AND. (LOCS(1).GT.0)) THEN
            CALL RCOPY (1, VALUES(LOCS(1)), MAXBLN)
            MAXBLN = -ABS(MAXBLN)
         ELSE
            MAXBLN = -1.0
            END IF
         LOCS(1) = 1
         CALL RCOPY (1, MAXBLN, VALUES(1))
C                                       Record Max Baseline in header
         CALL CATKEY ('WRIT', DISKUV, CNOUV, 'MAXABSU', 1, LOCS,
     *      VALUES, 2, IBUFF, I)
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' COPYING CATBLK DISK, CNO',I3,I6)
 1050 FORMAT ('SETDO3: CENTERED',I3,' OFFSET',I3,' IN',I3,' MODELS',
     *   ' 3DIMAG INCONSISTENT')
      END
