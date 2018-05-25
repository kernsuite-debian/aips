      SUBROUTINE GETANT (DISK, CNO, SUBA, CATBLK, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Reads AN table and stores the info in common.
C# EXT-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2001, 2003, 2007, 2009, 2011-2012
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
C   GETANT reads an antennas (AN) extension file and saves the
C   information in a common in DANS.INC.
C   The antenna latitude and longitude are strictly correct only for
C   the VLA and VLBI.  For other arrays the value returned will
C   be the value at the array center. (note: VLBI means here that the
C   antenna locations are given in an earth centered system.  Also the
C   handedness of the system is opposite the array centered scheme.)
C   NOTE: Uses AIPS LUN 40 for read the antenna file.
C   Inputs:
C      DISK     I        Volume number
C      CNO      I        Catalog slot number
C      SUBA     I        Subarray number (AN version number)
C      CATBLK   I(256)   Catalog header block
C   Outputs sent to COMMON (DANS.INC):
C      NSTNS    I        Maximum antenna number
C      STNNAM   C(*)*8   Antenna names
C      TELNO    I(*)     Antenna number: TELNO(i) = i
C      STNX     D(*)     X (meters)
C      STNY     D(*)     Y (meters)
C      STNZ     D(*)     Z (meters)
C      STNLAT   D(*)     Antenna latitude (rad).
C      STNLON   D(*)     Antenna east longitude (rad).
C      STNRAD   D(*)     Antenna radius from earth center (meter)
C      STNEPL   R(2,*)   Feed real/elipticity (poln, IF)
C      STNORI   R(2,*)   Feed imag/orientation (poln, IF)
C      STNPST   C*8      Feed solution type:
C                           'APPROX  ' => linear approximation
C                           'ORI-ELP ' => orientation-ellipticity
C      TIMLAB   C*8      Time system label (e.g. 'IAT', 'UTC')
C      ANTUTC   R        UT1-UTC (time sec)
C      ANTIAT   R        Data time - UTC (sec)
C      GSTIAT   D        GST (rad) at IAT=0 on reference date.
C      ROTIAT   D        Rotation rate of the earth in IAT (Radians/day)
C      ANTUTC   R        UT1-UTC (time sec)
C      ANTIAT   R        Data time - UTC (sec)
C      FQIDAN   I        FQID for which polzn properties determined.
C   Output:
C     BUFFER    I(*)     I/O buffer
C     IERR      I        Error code: 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, SUBA, CATBLK(256), BUFFER(*), IERR
C
      CHARACTER CHSOLT*8, CHELP*8
      INTEGER   IVER, LUN, LOOP, NIF, IANT, LOCS, KEYTYP, JERR, NR, I
      LOGICAL   DOELP
      DOUBLE PRECISION X, Y, Z, RHO, ARRLON
      HOLLERITH HSTNPS(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOLT, CHELP /'POLTYPE ','ORI-ELP'/
      DATA LUN /40/
C-----------------------------------------------------------------------
C                                       init the values
      NSTNS = 0
      FQIDAN = 0
      ANTIAT = 0.0
      ANTUTC = 0.0
      POLARX = 0.0
      POLARY = 0.0
      STNPST = ' '
      TIMLAB = ' '
      CNTRX = 0.0D0
      CNTRY = 0.0D0
      CNTRZ = 0.0D0
      GSTIAT = 0.0D0
      ROTIAT = 0.0D0
      DO 10 I = 1,MAXANT
         TELNO(I) = I
         MNTYP(I) = 0
         ANDIAM(I) = 0.0
         CALL RFILL (MAXIF, 0.0, ANFWHM(1,I))
         CALL RFILL (2*MAXIF, 0.0, STNELP(1,1,I))
         STNNAM(I) = ' '
         STNX(I) = 0.0D0
         STNY(I) = 0.0D0
         STNZ(I) = 0.0D0
         STNLAT(I) = 0.0D0
         STNLON(I) = 0.0D0
         STNRAD(I) = 0.0D0
 10      CONTINUE
C                                      Open AN extension file.
      IVER = SUBA
      IERR = 0
      CALL ANTINI ('READ', BUFFER, DISK, CNO, IVER, CATBLK, LUN, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      NR = BUFFER(5)
      NSTNS = 0
C                                       Get polarization solution type.
      STNPST = ' '
      MSGSUP = 32000
      CALL TABKEY ('READ', CHSOLT, 1, BUFFER, LOCS, HSTNPS, KEYTYP,
     *   JERR)
      MSGSUP = 0
      IF (JERR.EQ.0) CALL H2CHR (8, 1, HSTNPS, STNPST)
      DOELP = STNPST.EQ.CHELP
      CNTRX = ARRAYC(1)
      CNTRY = ARRAYC(2)
      CNTRZ = ARRAYC(3)
      GSTIAT = GSTIA0 * DG2RAD
      ROTIAT = DEGPDY * DG2RAD
      TIMLAB = TIMSYS
      ANTUTC = UT1UTC
      ANTIAT = DATUTC
      POLARX = POLRXY(1)
      POLARY = POLRXY(2)
      FQIDAN = ANFQID
C                                      Read AN records
      DO 200 IANT = 1,NR
         IANRNO = IANT
         CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Skip records with
C                                       zero station numbers
         IF ((NOSTA.LE.0) .OR. (NOSTA.GT.MAXANT)) THEN
            WRITE (MSGTXT,1120) NOSTA, IANT, IVER
            CALL MSGWRT (6)
            GO TO 200
            END IF
C
         NSTNS = MAX (NSTNS, NOSTA)
         STNNAM(NOSTA) = ANNAME
         TELNO(NOSTA)  = NOSTA
         ANDIAM(NOSTA) = DIAMAN
         STNX(NOSTA)   = STAXYZ(1)
         STNY(NOSTA)   = STAXYZ(2)
         STNZ(NOSTA)   = STAXYZ(3)
         IF ((STAXYZ(1).EQ.0.0) .AND. (STAXYZ(2).EQ.0.0))
     *      STNY(NOSTA) = 1.0D0
         IF (STAXYZ(3).EQ.0.0) STNZ(NOSTA) = 1.0D0
C                                       X => (lat 0, long 0)
C                                       Y => (lat 0, long 90E)
C                                       Z => (lat 90)
C                                       Get antenna latitude and
C                                       east longitude.
         X = CNTRX
         Y = CNTRY
         Z = CNTRZ
         IF ((ABS(X).GT.1.D2) .AND. (ABS(Y).GT.1.D2) .AND.
     *      (ABS(Z).GT.1.D2)) THEN
            ARRLON = ATAN2 (Y, X)
            X = X + STAXYZ(1)*COS (ARRLON) - STAXYZ(2)*SIN (ARRLON)
            Y = Y + STAXYZ(2)*COS (ARRLON) + STAXYZ(1)*SIN (ARRLON)
            Z = Z + STAXYZ(3)
         ELSE
            X = X + STAXYZ(1)
            Y = Y + STAXYZ(2)
            Z = Z + STAXYZ(3)
            END IF
         RHO = SQRT (X*X + Y*Y + Z*Z)
         STNLAT(NOSTA) = 0.0
         STNLON(NOSTA) = 0.0
         IF (RHO.NE.0.D0) STNLAT(NOSTA) = ASIN (Z / RHO)
         IF ((X.NE.0.D0) .OR. (Y.NE.0.0D0)) STNLON(NOSTA) = ATAN2 (Y, X)
         STNRAD(NOSTA) = RHO
         MNTYP(NOSTA) = MNTSTA
C                                       Feed polarizations, FWHM
         LOOP = -1
         DO 180 NIF = 1,ANTNIF
            LOOP = LOOP + 2
            ANFWHM(NIF,NOSTA) = FWHMAN(NIF)
            STNELP(1,NIF,NOSTA) = POLCA(LOOP)
            STNORI(1,NIF,NOSTA) = POLCA(LOOP+1)
            STNELP(2,NIF,NOSTA) = POLCB(LOOP)
            STNORI(2,NIF,NOSTA) = POLCB(LOOP+1)
C                                       Default circular poln
            IF ((ABS (POLCA(LOOP)).LT.1.0E-10) .AND. DOELP) THEN
               STNELP(1,NIF,NOSTA) =  0.785398164
               STNELP(2,NIF,NOSTA) = -0.785398164
               STNORI(1,NIF,NOSTA) = 0.0
               STNORI(2,NIF,NOSTA) = 0.0
               END IF
 180        CONTINUE
 200     CONTINUE
C                                      Close AN extension file
      CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1200) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETANT: ERROR',I3,' OPEN-FOR-READ AN FILE')
 1100 FORMAT ('GETANT: ERROR',I3,' READING AN FILE')
 1120 FORMAT ('GETANT: STATION NUMBER',I6,' FOR ENTRY=',I4,' SUBA=',I3)
 1200 FORMAT ('GETANT: ERROR',I3,' CLOSING AN FILE')
      END
