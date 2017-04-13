      SUBROUTINE UNSETG (IBUFF)
C-----------------------------------------------------------------------
C! Unsets the UV model computation file status
C# UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1999
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
C   UNSETG undoes the file status created by SETGDS.
C   In/out:
C      IBUFF    I(512)    Work buffer
C-----------------------------------------------------------------------
      INTEGER   IBUFF(*)
C
      CHARACTER ITYPE*2, NDUM*12, CDUM*6
      INTEGER   I, JJ, DUM, IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Loop thru fields.
      DO 40 JJ = 1,MFIELD
         I = MFIELD + 1 - JJ
         ITYPE = '  '
         CALL CATDIR ('CSTA', CCDISK(I), CCCNO(I), NDUM, CDUM, DUM,
     *       ITYPE, NLUSER, 'CLRD', IBUFF, IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.10)) THEN
            WRITE (MSGTXT,1000) IRET, CCDISK(I), CCCNO(I)
            CALL MSGWRT (6)
            END IF
 40      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' CLEARING DISK',I3,' CNO',I5)
      END
