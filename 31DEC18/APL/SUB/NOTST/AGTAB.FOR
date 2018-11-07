      SUBROUTINE AGTAB (OPCODE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! I/O to array geometry tables (following initialization by AGINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009
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
C   Does I/O to array geometry tables after setup by AGINI
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(*)       I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          AGINI.
C   Inputs from common:
C      IAGRNO   I          Next scan number to read or write.
C      AGKOLS   I(MAXAGC)  The column pointer array in order, ANNAME,
C                          STABXYZ, DERXYZ, ORBPARM, NOSTA, MNTSTA,
C                          STAXOF
C      AGNUMV   I(MAXAGC)  Element count in each column.
C   Input/output: (written to or read from antenna file)
C      ANNAME   C*8        Station name
C      STAXYZ   D(3)       X,Y,Z offset from array center
C      DERXYZ   D(3)       1st derivative of X,Y,Z offsets.
C      ORBPRM   D(?)       Orbital parameters.
C      NOSTA    I          Station number
C      MNTSTA   I          Mount type, 0=altaz, 1=equatorial, 2=orbiting
C      STAXOF   R(3)       Axis offset
C   Output:
C      IAGRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DAGV.INC' for the declarations in
C   AGINI and AGTAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IERR
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   NCOPY, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAGV.INC'
      INTEGER   RECI(MAXREC)
      HOLLERITH RECH(MAXREC)
      REAL      RECORD(MAXREC)
      DOUBLE PRECISION RECD(MAXRCD)
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Antenna name
         CALL CHR2H (8, ANNAME, 1, RECH(AGKOLS(KANN)))
C                                       Antenna location
         NCOPY = AGNUMV(KSTA)
         DO 10 I = 1,NCOPY
            RECD(AGKOLS(KSTA)+I-1) = STAXYZ(I)
 10         CONTINUE
C                                       1st derivatives of antenna
C                                       locations
         NCOPY = AGNUMV(KDER)
         DO 20 I = 1,NCOPY
            RECD(AGKOLS(KDER)+I-1) = DERXYZ(I)
 20         CONTINUE
C                                       Orbital parameters
         NCOPY = AGNUMV(KORB)
         DO 30 I = 1,NCOPY
            RECD(AGKOLS(KORB)+I-1) = ORBPRM(I)
 30         CONTINUE
C                                       Antenna number
         RECI(AGKOLS(KNOS)) = NOSTA
C                                       Mount type
         RECI(AGKOLS(KMNT)) = MNTSTA
C                                       Axis offset
         NCOPY = AGNUMV(KSTX)
         DO 40 I = 1,NCOPY
            RECORD(AGKOLS(KSTX)+I-1) = STAXOF(I)
 40         CONTINUE
C                                       diameter
         IF (AGNUMV(KDIA).GT.0) RECORD(AGKOLS(KDIA)) = STADIA
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IAGRNO, RECORD, BUFFER, IERR)
      IAGRNO = IAGRNO + 1
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Antenna name
         CALL H2CHR (8, 1, RECH(AGKOLS(KANN)), ANNAME)
C                                       Antenna location
         NCOPY = AGNUMV(KSTA)
         DO 60 I = 1,NCOPY
            STAXYZ(I) = RECD(AGKOLS(KSTA)+I-1)
 60         CONTINUE
C                                       1st derivatives of antenna
C                                       locations
         NCOPY = AGNUMV(KDER)
         DO 70 I = 1,NCOPY
            DERXYZ(I) = RECD(AGKOLS(KDER)+I-1)
 70         CONTINUE
C                                       Orbital parameters
         NCOPY = AGNUMV(KORB)
         DO 80 I = 1,NCOPY
            ORBPRM(I) = RECD(AGKOLS(KORB)+I-1)
 80         CONTINUE
C                                       Antenna number
         NOSTA = RECI(AGKOLS(KNOS))
C                                       Mount type
         MNTSTA = RECI(AGKOLS(KMNT))
C                                       Axis offset
         NCOPY = AGNUMV(KSTX)
         DO 90 I = 1,NCOPY
            STAXOF(I) = RECORD(AGKOLS(KSTX)+I-1)
 90         CONTINUE
C                                       diameter
         IF (AGNUMV(KDIA).GT.0) THEN
            STADIA = RECORD(AGKOLS(KDIA))
         ELSE
            STADIA = 0.0
            END IF
         END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IAGRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('AGTAB: TABIO ERROR',I5)
      END
