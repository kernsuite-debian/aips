      SUBROUTINE H2WAWA (NAME, CLASS, SEQ, PTYPE, VOL, USID, NAMEST)
C-----------------------------------------------------------------------
C! packs AIPS adverb values into WaWa IO NameString
C# IO-WaWa
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
C   packs AIPS adverb values (Holleriths, floating points) into a WaWa
C   IO Namestring having format A12, A6, A2, I7, I2, I7 for name, class,
C   ptype, seq, vol, usid
C   Inputs:
C      NAME    H(3)   file name
C      CLASS   H(2)   file class (6 chars)
C      SEQ     R      file sequence number
C      PTYPE   H      file physical type (2 chars)
C      VOL     R      file disk number
C      USID    R      user number
C   Output:
C      NAMEST  C*36   WaWa Namestring
C-----------------------------------------------------------------------
      HOLLERITH NAME(3), CLASS(2), PTYPE
      REAL      SEQ, VOL, USID
      CHARACTER NAMEST*36
C
      CHARACTER CNAME*12, CCLASS*6, CPTYPE*2
      INTEGER   IROUND, ISEQ, IVOL, IUSID
C-----------------------------------------------------------------------
      CALL H2CHR (12, 1, NAME, CNAME)
      CALL H2CHR (6, 1, CLASS, CCLASS)
      CALL H2CHR (2, 1, PTYPE, CPTYPE)
      ISEQ = IROUND (SEQ)
      IVOL = IROUND (VOL)
      IUSID = IROUND (USID)
      WRITE (NAMEST,1000,ERR=999) CNAME, CCLASS, CPTYPE, ISEQ, IVOL,
     *   IUSID
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A12,A6,A2,I7,I2,I7)
      END
