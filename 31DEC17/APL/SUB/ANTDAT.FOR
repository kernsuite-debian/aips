      SUBROUTINE ANTDAT (CNO, VOL, IABUF, NANT, XDAT, XFREQ)
C-----------------------------------------------------------------------
C! Returns the reference date and frequency for each array in uv dataset
C# UV-util EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2009
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
C   ANTDAT return the JD reference date and frequency for each array
C   (AN file) in a uv data base.  NOTE: the catalog header is accessed
C   thru commons /MAPHDR/ and /UVHDR/ which must be filled before
C   calling ANTDAT (use UVPGET for the latter).
C   Inputs:
C      CNO     I        Cat. slot number of uv data base.
C      VOL     I        Vol. number of uv data base.
C   Output:
C      IABUF   I(512)   Work buffer
C      NANT    I        Number of AN files (arrays) assoc w data base
C      XDAT    D(*)     Mod. JD reference date, index => array no.
C      XFREQ   D(*)     Frequency (GHz), index => array no.
C-----------------------------------------------------------------------
      INTEGER   CNO, VOL, NANT, IABUF(512)
      DOUBLE PRECISION XDAT(*), XFREQ(*)
C
      INTEGER   LUN, I, IERR
      CHARACTER READ*4, CHTEMP*8, CLOSE*4
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA READ, CLOSE /'READ', 'CLOS'/
      DATA LUN /29/
C-----------------------------------------------------------------------
C                                        Get first date and freq.
      CALL H2CHR (8, 1, CATH(KHDOB), CHTEMP)
      CALL JULDAY (CHTEMP, XDAT(1))
      XFREQ(1) = FREQ * 1.0D-9
C                                        Find number AN files.
      CALL FNDEXT ('AN', CATBLK, NANT)
      IF (NANT.LE.0) GO TO 999
C                                        Loop thru extension files.
      DO 50 I = 1,NANT
         IF (I.NE.1) XFREQ(I) = 0.0D0
         IF (I.NE.1) XDAT(I) = 0.0D0
         CALL ANTINI (READ, IABUF, VOL, CNO, I, CATBLK, LUN, IANRNO,
     *      ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *      POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME,
     *      NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
C                                        Get date and Freq.
         IF (IERR.EQ.0) THEN
            IF (I.NE.1) THEN
               CALL JULDAY (RDATE, XDAT(I))
               XFREQ(I) = SAFREQ * 1.0D-9
               END IF
            CALL TABAN (CLOSE, IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            END IF
 50      CONTINUE
C
 999  RETURN
      END
