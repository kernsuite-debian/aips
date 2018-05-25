      SUBROUTINE TABOB (OPCODE, BUFFER, IOBRNO, OBKOLS, OBNUMV,
     *   IANTOB, ISUBOB, DTIMOB, DXYZOB, DVELOB, ANGLOB, ECLPOB,
     *   ORIOB, IERR)
C----------------------------------------------------------------------
C! I/O to spacecraft orbit tables (following init. by OBINI)
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2007, 2013
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
C----------------------------------------------------------------------
C   Does I/O to spacecraft orbit tables. Usually used after setup
C   by OBINI.
C   Inputs:
C      OPCODE       C*4         Operation code:
C                               'READ' = read entry from table
C                               'WRIT' = write entry in table
C                               'CLOS' = close file, flush on write
C      BUFFER       I(*)        I/O buffer and related storage, also
C                               defines file if open. Should have
C                               been returned by OBINI.
C      OBKOLS       I(MAXOBC)   The column pointer array in order:
C                               ANTENNA NO, SUBARRAY, TIME,
C                               ORBXYZ, VELXYZ, SUN ANGLE, ECLIPSE,
C                               ORIENTATION
C      OBNUMV       I(MAXOBC)   Element count in each column
C   Input/output:
C      IOBRNO       I           Next row to read or write
C      IANTOB       I           Antenna number
C      ISUBOB       I           Subarray number
C      DTIMOB       D           Time (days wrt reference day)
C      DXYZOB       D(3)        Spacecraft position (x, y, z)
C      DVELOB       D(3)        Spacecraft velocity (vx, vy, vz)
C      ANGLOB       R(3)        Sun angle (deg)
C      ECLPOB       R(4)        Eclipse times (days wrt ref. day)
C      ORIOB        R           Orientation (deg)
C   Output:
C      IERR         I           Return code (0=>ok; else error)
C                               Note: -1 => read by not selected
C----------------------------------------------------------------------
      INCLUDE 'INCS:POBV.INC'
      CHARACTER OPCODE*4
      DOUBLE PRECISION DXYZOB(3), DVELOB(3), DTIMOB
      REAL      ANGLOB(3), ECLPOB(4), ORIOB
      INTEGER   BUFFER(512), IOBRNO, OBKOLS(MAXOBC), OBNUMV(MAXOBC),
     *   IANTOB, ISUBOB, IERR
C
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   DOREAD, DOCLOS
      DOUBLE PRECISION RECD(MXDPOB)
      REAL      RECORD(MXSPOB)
      INTEGER   RECI(MXSPOB)
      EQUIVALENCE (RECD, RECORD, RECI)
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Check OPCODE
      DOREAD = (OPCODE.EQ.'READ')
      DOCLOS = (OPCODE.EQ.'CLOS')
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Antenna number
         RECI(OBKOLS(OBIANT)) = IANTOB
C                                       Subarray number
         RECI(OBKOLS(OBISUB)) = ISUBOB
C                                       Time
         RECD(OBKOLS(OBDTIM)) = DTIMOB
C                                       Spacecraft position, velocity
C                                       and sun angle.
         CALL DPCOPY (OBNUMV(OBDPOS), DXYZOB, RECD(OBKOLS(OBDPOS)))
         CALL DPCOPY (OBNUMV(OBDVEL), DVELOB, RECD(OBKOLS(OBDVEL)))
         CALL RCOPY (OBNUMV(OBRANG), ANGLOB, RECORD(OBKOLS(OBRANG)))
C                                       Eclipse tags
         CALL RCOPY (OBNUMV(OBRECL), ECLPOB, RECORD(OBKOLS(OBRECL)))
C                                       Spacecraft orientation
         RECORD(OBKOLS(OBRORI)) = ORIOB
         END IF
C                                       Process record
200   CALL TABIO (OPCODE, 0, IOBRNO, RECORD, BUFFER, IERR)
      IOBRNO = IOBRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'TABOB', IERR)
         GO TO 999
         END IF
C                                       If READ pick data from record
      IF (DOREAD) THEN
C                                       Antenna number
         IANTOB = RECI(OBKOLS(OBIANT))
C                                       Subarray number
         ISUBOB = RECI(OBKOLS(OBISUB))
C                                       Time
         DTIMOB = RECD(OBKOLS(OBDTIM))
C                                       Spacecraft position, velocity
C                                       and sun angle
         CALL DPCOPY (OBNUMV(OBDPOS), RECD(OBKOLS(OBDPOS)), DXYZOB)
         CALL DPCOPY (OBNUMV(OBDVEL), RECD(OBKOLS(OBDVEL)), DVELOB)
         CALL RCOPY (OBNUMV(OBRANG), RECORD(OBKOLS(OBRANG)), ANGLOB)
C                                       Eclipse tags
         CALL RCOPY (OBNUMV(OBRECL), RECORD(OBKOLS(OBRECL)), ECLPOB)
C                                       Spacecraft orientation
         ORIOB = RECORD(OBKOLS(OBRORI))
         END IF
      GO TO 999
C                                       Close
800   CALL TABIO ('CLOS', 0, IOBRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'TABOB', IERR)
         GO TO 999
         END IF
      GO TO 999
C                                       Exit
999   RETURN
      END
