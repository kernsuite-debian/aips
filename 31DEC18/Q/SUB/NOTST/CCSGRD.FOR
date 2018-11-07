      SUBROUTINE CCSGRD (IFIELD, DOSUM, SCRGRD, SCRWRK, JBUFSZ, BUFF1,
     *   BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Transforms CLEAN components to a grid.
C# AP-util Map UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2001, 2006, 2008, 2015
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
C   CCSGRD transforms CLEAN components to GRD file.
C   GRD file is zeroed if IFIELD=1 before filling.
C   Input:
C      IFIELD   I      FIELD number for grid.
C      DOSUM    L      If true sum the flux in each field
C      SCRGRD   I      /CFILES/ file number for grid file.
C      SCRWRK   I      /CFILES/ file number for work file
C      JBUFSZ   I      Size of the buffers in bytes. The dimension of
C                      the buffers must be at least 4096.
C      MFREQ    I      Current frequency channel.
C      CLEAN components for field IFIELD.
C   Output:
C      BUFF1    R(*)   Work buffer
C      BUFF2    R(*)   Work buffer
C      BUFF3    R(*)   Work buffer
C      IRET     I      Error return
C   Transform of additional CLEAN components left in GRD file.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, SCRGRD, SCRWRK, JBUFSZ, IRET
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
      LOGICAL   DOSUM
C
      INTEGER   FIRST, NUMBER, MAXCMP, NUMCLN, JNUM, NAPGAU, NAPEX1,
     *   NAPGRD, NAPCMP, NAPCMS, APBUF, ONENY, TWONY, WRK1, WRK2, NWORD,
     *   APSIZ, FLIST(22,2), NLOAD, I, JLIM, NX, NY, IT1
      LOGICAL   T, F, WFIRST, DOGAUS
      REAL      GPARMS(3), DU, DV, UU
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA APBUF / 1 /
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      WFIRST = T
      NX = FLDSZ(1,IFIELD) * OSFX + 0.1
      NY = FLDSZ(2,IFIELD) * OSFY + 0.1
C                                       Initialize flux sums.
      IF (DOSUM) FLUXG(IFIELD) = 0.0
C                                       Determine Area assignments in AP
      ONENY = NY
      TWONY = 2 * NY
      NAPGRD = 100
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
      NAPCMP = NAPGRD + WRK2
      NAPGAU = NAPCMP + WRK2
      NAPEX1 = NAPGAU + WRK1
      NAPCMS = NAPEX1 + WRK1
C                                       Set up for APIO
      CALL FILL (22, 0, FLIST)
C                                       LUNs
      FLIST(1,1) = LUNS(4)
C                                       /CFILES/ numbers
      FLIST(2,1) = SCRGRD
C                                       Size
      FLIST(5,1) = NY * 2
      FLIST(6,1) = NX / 2 + 1
C                                       Buffer size
      FLIST(13,1) = JBUFSZ
C                                       Second file
      CALL COPY (22, FLIST(1,1), FLIST(1,2))
      FLIST(1,2) = LUNS(5)
      FLIST(2,2) = SCRWRK
C                                       Determine maximum number of comp
C                                       per pass.
      APSIZ = PSAPNW * 1024
      MAXCMP = (APSIZ - NAPCMS) / 5
      NUMCLN = NCLNG(IFIELD) - NSUBG(IFIELD) + 1
      FIRST = NSUBG(IFIELD)
      APBUF = 2
C                                       Determine size of AP buffer for
      NLOAD = NAPGAU - 3
C                                       Begin component loop.
 10      NUMBER = MIN (MAXCMP, NUMCLN)
C                                       Load CLEAN components this pass.
         JNUM = NUMBER
         CALL GRDCRM (IFIELD, DOSUM, NAPCMS, APBUF, FIRST, NUMBER,
     *      NLOAD, GPARMS, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Gaussian model?
         DOGAUS = (GPARMS(1).GT.0.0) .AND. (GPARMS(2).GT.0.0)
         IF (DOGAUS) THEN
            IF ((NX*CELLSG(1).EQ.0.0) .OR. (NY*CELLSG(2).EQ.0.0)) THEN
               IRET = 8
               MSGTXT = 'NX, NY, OR CELLSG 0!!!'
               CALL MSGWRT (8)
               GO TO 999
               END IF
            DU = RAD2AS / (NX * ABS(CELLSG(1)))
            DV = RAD2AS / (NY * ABS(CELLSG(2)))
            END IF
C                                       Begin loop thru map.
         JLIM = NX / 2 + 1
         DO 150 I = 1,JLIM
C                                       If not first pass read old grid
            CALL QWR
            IF (.NOT.WFIRST) THEN
               CALL APIO ('READ', FLIST(1,1), NAPGRD, BUFF2, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1080) IRET,I
                  GO TO 990
                  END IF
C                                       Clear grid.
            ELSE
               CALL QVCLR (NAPGRD, 1, TWONY)
               CALL QWR
               END IF
            CALL QWD
C                                       Gaussian taper then crunch
            IF (DOGAUS) THEN
               UU = DU * (I-1)
               CALL QTAPER (GPARMS, UU, DV, ONENY, NAPGAU)
               CALL QGRDCC (NAPCMS, NAPGAU, NAPCMP, NAPGRD, ONENY,
     *            NUMBER)
C                                       Crunch data.
            ELSE
               CALL QGRDCC (NAPCMS, -1, NAPCMP, NAPGRD, ONENY, NUMBER)
               END IF
C                                       Write file back to disk.
            CALL QWR
            CALL APIO ('WRIT', FLIST(1,2), NAPGRD, BUFF1, IRET)
            IF (IRET.EQ.0) GO TO 150
               WRITE (MSGTXT,1090) IRET, I
               GO TO 990
 150        CONTINUE
C                                       End of this pass, close files.
         CALL APIO ('CLOS', FLIST(1,1), NAPGRD, BUFF2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            CALL MSGWRT (8)
            END IF
         CALL APIO ('CLOS', FLIST(1,2), NAPGRD, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            CALL MSGWRT (8)
            END IF
         FIRST = FIRST + JNUM
         NUMCLN = NUMCLN - JNUM
         WFIRST = F
C                                       Switch WRK and GRID files.
         IT1 = SCRWRK
         SCRWRK = SCRGRD
         SCRGRD = IT1
         FLIST(2,1) = SCRGRD
         FLIST(2,2) = SCRWRK
C                                       Check if time for roller
C                                       Check for more CLEAN components.
         NWORD = APSIZ
         IF (FIRST.LE.NCLNG(IFIELD)) THEN
            CALL QROLL (NWORD, BUFF3, JBUFSZ, IRET)
            IF (IRET.NE.0) GO TO 999
            GO TO 10
            END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('CCSGRD: WRITE ERROR ',I3,' GRID ROW ',I6)
 1080 FORMAT ('CCSGRD: ERROR ',I3,' READ GRID ROW ',I6)
 1150 FORMAT ('CCSGRD: ERROR ',I3,' CLOSING GRID FILE')
      END
