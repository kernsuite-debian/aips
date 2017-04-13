      SUBROUTINE MAPCLS (OP, IVOL, CNO, LUN, IND, CATBLK, CATUP,
     *   WBUFF, IERR)
C-----------------------------------------------------------------------
C! closes cataloged file, updating header and catalog status if needed
C# IO-util Catalog
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
C   closes a cataloged file, updates header on disk, clears catalog
C   status
C   Inputs:
C      OP      C*4     OPcode used by MAPOPN to open this file
C      IVOL    I       Disk volume containing map file
C      CNO     I       Catalog slot number of file
C      LUN     I       Logical unit # used for file
C      IND     I       FTAB pointer for LUN
C      CATBLK  I(256)  New catalog header which can optionally
C                      be written into header if OP=WRIT or INIT
C                      Dummy arguement if OP=READ
C      CATUP   L       If TRUE, write CATBLK into catalog,
C                      ignored if OP = READ
C   Outputs:
C      IERR    I       0 = O.K.
C                      1 = CATDIR couldn't access catalog
C                      5 = illegal OP code
C-----------------------------------------------------------------------
      INTEGER   IVOL, CNO, LUN, IND, CATBLK(256), WBUFF(256), IERR
      LOGICAL   CATUP
      CHARACTER OP*4
C
      CHARACTER  CHTM12*12, CHTM6*6, CHTM2*2, UNCODE(3)*4, OPCODE(3)*4
      INTEGER   IER, IOP, NOP, ERROP, DUM
      DATA OPCODE /'READ', 'WRIT', 'INIT'/
      DATA UNCODE /'CLRD', 'CLWR', 'CLWR'/
      DATA ERROP, NOP /5, 3/
C-----------------------------------------------------------------------
      IERR = 0
      DO 10 IOP = 1,NOP
         IF (OP.EQ.OPCODE(IOP)) GO TO 20
 10      CONTINUE
      IERR = ERROP
      GO TO 999
C                                       update catalog entry
 20   IF ((.NOT.CATUP) .OR. (IOP.EQ.1)) GO TO 30
         CALL CATIO ('UPDT', IVOL, CNO, CATBLK, UNCODE(IOP), WBUFF, IER)
         IF ((IER.NE.0) .AND. (IER.NE.9)) IERR = 1
         GO TO 40
C                                       update directory
 30   CONTINUE
         CHTM12 = ' '
         CHTM6 = ' '
         CHTM2 = ' '
         DUM = 0
         CALL CATDIR ('CSTA', IVOL, CNO, CHTM12, CHTM6, DUM, CHTM2, DUM,
     *      UNCODE(IOP), WBUFF, IER)
         IF ((IER.NE.0) .AND. (IER.NE.10)) IERR = 1
C                                       close map file
 40   CALL ZCLOSE (LUN, IND, IER)
C
 999  RETURN
      END
