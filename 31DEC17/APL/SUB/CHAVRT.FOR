      SUBROUTINE CHAVRT (IDIR, NCH, IBUF, HBUF)
C-----------------------------------------------------------------------
C! converts between local HOLL and local INT binary forms for transport
C# FITS TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   CHAVRT converts between ascii-standard characters in local format
C   integers (suitable for ZILI16 and transport) and local HOLLERITH
C   Inputs:
C      IDIR   I      1 => local HOLL -> standard -> local INT
C                    2 => local INT -> standard -> local HOLL
C      NCH    I      number of characters
C   In/out:
C      IBUF   I(*)   transport buffer (* = NCH/2)
C      HBUF   H*(*)  local AIPS HOLLERITH string
C-----------------------------------------------------------------------
      HOLLERITH HBUF(*)
      INTEGER   IDIR, NCH, IBUF(*)
C
      CHARACTER CBUF*64
      INTEGER   IS, IXT(20)
      HOLLERITH XT(20)
      EQUIVALENCE (IXT, XT)
C-----------------------------------------------------------------------
C                                       local char to integer
      IF (NCH.LE.0) GO TO 999
      IS = (NCH - 1) / 2 + 1
      IF (IDIR.LE.1) THEN
         CALL H2CHR (NCH, 1, HBUF, CBUF)
         CALL ZCLC8 (NCH, CBUF, 1, XT)
         CALL ZI16IL (IS, 1, IXT, IBUF)
      ELSE
         CALL ZILI16 (IS, IBUF, 1, IXT)
         CALL ZC8CL (NCH, 1, XT, CBUF)
         CALL CHR2H (NCH, CBUF, 1, HBUF)
         END IF
C
 999  RETURN
      END
