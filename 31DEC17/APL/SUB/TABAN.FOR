      SUBROUTINE TABAN (OPCODE, BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *   STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA,
     *   POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
C-----------------------------------------------------------------------
C! I/O to antenna tables (following initialization by ANTINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008-2009, 2013, 2016
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
C   Does I/O to Antenna tables. Usually used after setup by ANTINI.
C   Inputs:
C      OPCODE   C*4     Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C      BUFFER   I(512)  I/O buffer and related storage, also defines
C                       file if open. Should have been returned by
C                       ANTINI or TABINI.
C      IANRNO   I       Next scan number to read or write.
C      ANKOLS   I(14)   The column pointer array in order, ANNAME,
C                       STABXYZ, ORBPARM, NOSTA, MNTSTA, STAXOF, DIAMAN,
C                       FWHMAN, POLTYA, POLAA, POLCALA, POLTYB, POLAB,
C                       POLCALB
C      ANNUMV   I(14)   Element count in each column.
C   Input/output: (written to or read from antenna file)
C      ANNAME   C*8     Station name
C      STAXYZ   D(3)    X,Y,Z offset from array center
C      ORBPRM   D(?)    Orbital parameters.
C      NOSTA    I       Station number
C      MNTSTA   I       Mount type, 0=altaz, 1=equatorial, 2=orbiting
C                       3=X-Y (east-west, Hobart), 6 = array,
C                       4=Right-handed Nasmyth, 5=Left-handed Nasmyth
C      STAXOF   R       Axis offset
C      DIAMAN   R       Antenna diameter in meters
C      FWHMAN   R(*)    FWHM (degrees/meter) of single dish each IF
C                       scales as wavelength
C      POLTYA   C*2     Feed A feed poln. type 'R','L','X','Y'
C      POLAA    R       Feed A feed position angle.
C      POLCA    R(?)    Feed A poln. cal parameter. (note 2)
C      POLTYB   C*2     Feed B feed poln. type 'R','L','X','Y'
C      POLAB    R       Feed B feed position angle.
C      POLCB    R(?)    Feed B poln. cal parameters.
C   Output:
C      IANRNO   I       Next GAIN number.
C      IERR     I       Error code, 0=>OK else TABIO error.
C                       Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DANT.INC' for the declarations in
C   ANTINI and TABAN.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER ANBSIZ
      PARAMETER (ANBSIZ=5*MAXIF+50)
C                                       Any changes here should also be
C                                       done in DANT.INC
      INTEGER   NOSTA, MNTSTA, ANKOLS(MAXANC), ANNUMV(MAXANC), IANRNO
      CHARACTER ANNAME*8, POLTYA*2, POLTYB*2
      REAL      STAXOF, DIAMAN, FWHMAN(MAXIF), POLAA, POLCA(2*MAXIF),
     *   POLAB, POLCB(2*MAXIF)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), IERR
C
      INTEGER   RECI(ANBSIZ), KOLS(MAXANC), NAMKOL, STAKOL, ORBKOL,
     *   NOSKOL, MNTKOL, XOFKOL, DIAKOL, FWKOL, TPAKOL, PLAKOL, PCAKOL,
     *   TPBKOL, PLBKOL, PCBKOL, NCOPY
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(ANBSIZ)
      HOLLERITH RECH(ANBSIZ)
      DOUBLE PRECISION RECD(ANBSIZ/2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (KOLS(1), NAMKOL), (KOLS(2), STAKOL),
     *   (KOLS(3), ORBKOL), (KOLS(4),NOSKOL), (KOLS(5), MNTKOL),
     *   (KOLS(6), XOFKOL), (KOLS(7), DIAKOL), (KOLS(8), FWKOL),
     *   (KOLS(9), TPAKOL), (KOLS(10), PLAKOL),(KOLS(11), PCAKOL),
     *   (KOLS(12),TPBKOL), (KOLS(13), PLBKOL), (KOLS(14), PCBKOL)
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      CALL COPY (14, ANKOLS, KOLS)
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Antenna name
         CALL CHR2H (8, ANNAME, 1, RECH(NAMKOL))
C                                       Antenna location
         NCOPY = ANNUMV(2) * NWDPDP
         CALL RCOPY (NCOPY, STAXYZ, RECD(STAKOL))
C                                       Orbital parameters
         NCOPY = ANNUMV(3) * NWDPDP
         CALL RCOPY (NCOPY, ORBPRM, RECD(ORBKOL))
C                                       Antenna number
         RECI(NOSKOL) = NOSTA
C                                       Mount type
         RECI(MNTKOL) = MNTSTA
C                                       Axis offset
         RECORD(XOFKOL) = STAXOF
C                                       diameter
         IF (DIAKOL.GT.0) THEN
            RECORD(DIAKOL) = DIAMAN
            END IF
C                                       FWHM sd beam
         IF (FWKOL.GT.0) THEN
            NCOPY = ANNUMV(8)
            CALL RCOPY (NCOPY, FWHMAN, RECORD(FWKOL))
            END IF
C                                       Polarization type A
         RECORD(TPAKOL) = HBLANK
         CALL CHR2H (1, POLTYA, 1, RECH(TPAKOL))
C                                       Polarization angle A
         RECORD(PLAKOL) = POLAA
C                                       Polarization cal. const. A
         NCOPY = ANNUMV(11)
         CALL RCOPY (NCOPY, POLCA, RECORD(PCAKOL))
C                                       Polarization type B
         RECORD(TPBKOL) = HBLANK
         CALL CHR2H (1, POLTYB, 1, RECH(TPBKOL))
C                                       Polarization angle B
         RECORD(PLBKOL) = POLAB
C                                       Polarization cal. const. B
         NCOPY = ANNUMV(14)
         CALL RCOPY (NCOPY, POLCB, RECORD(PCBKOL))
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, IANRNO, RECORD, BUFFER, IERR)
      IANRNO = IANRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Antenna name
         CALL H2CHR (8, 1, RECH(NAMKOL), ANNAME)
C                                       Antenna location
         NCOPY = ANNUMV(2) * NWDPDP
         CALL RCOPY (NCOPY, RECD(STAKOL), STAXYZ)
C                                       Orbital parameters
         NCOPY = ANNUMV(3) * NWDPDP
         CALL RCOPY (NCOPY, RECD(ORBKOL), ORBPRM)
C                                       Antenna number
         NOSTA = RECI(NOSKOL)
C                                       Mount type
         MNTSTA = RECI(MNTKOL)
C                                       Axis offset
         STAXOF = RECORD(XOFKOL)
C                                       diameter
         IF (DIAKOL.GT.0) THEN
            DIAMAN = RECORD(DIAKOL)
         ELSE
            DIAMAN = 0.0
            END IF
C                                       FWHM sd beam
         IF (FWKOL.GT.0) THEN
            NCOPY = ANNUMV(8)
            CALL RCOPY (NCOPY, RECORD(FWKOL), FWHMAN)
         ELSE
            CALL RFILL (MAXIF, 0.0, FWHMAN)
            END IF
C                                       Polarization type A
         POLTYA = ' '
         CALL H2CHR (1, 1, RECH(TPAKOL), POLTYA)
C                                       Polarization angle A
         POLAA = RECI(PLAKOL)
C                                       Polarization cal. const. A
         NCOPY = ANNUMV(11)
         CALL RCOPY (NCOPY, RECORD(PCAKOL), POLCA)
C                                       Polarization type B
         POLTYB = ' '
         CALL H2CHR (1, 1, RECH(TPBKOL), POLTYB)
C                                       Polarization angle B
         POLAB = RECI(PLBKOL)
C                                       Polarization cal. const. B
         NCOPY = ANNUMV(14)
         CALL RCOPY (NCOPY, RECORD(PCBKOL), POLCB)
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IANRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABAN: TABIO ERROR',I5)
      END
