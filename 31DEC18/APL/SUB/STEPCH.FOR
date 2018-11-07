      SUBROUTINE STEPCH (EPOCH, INTEPC, TABEPC, STREPC, LABEPC)
C-----------------------------------------------------------------------
C! Sets up Coordinate transformation using Real epoch value
C# Math Coordinates
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
C   Sets up Coordinate transformation using Real epoch value
C   Inputs:
C      EPOCH    R      Epoch 
C   Outputs:
C      INTEPC   I      Integer Epoch index
C      TABEPC   C*8    Table Label For Epoch
C      STREPC   C*40   CRDTRN 40 chracter string name of Epoch
C      LABEPC   C*80   Label for Epoch
C   This procedure is used with CRDSET and CRDTRN
C-----------------------------------------------------------------------
      REAL      EPOCH
      INTEGER   INTEPC
      CHARACTER TABEPC*8, STREPC*40, LABEPC*80
C                                       Number of coordinate systems
      INTEGER   NCRD
      PARAMETER (NCRD=7)
      INTEGER   IROUND, I, J
      CHARACTER CRDNAM(NCRD)*40, TABEPO(NCRD)*8
      DATA CRDNAM/'EQUATORIAL B1950.0',
     *   'EQUATORIAL b1900.0',
     *   'EQUATORIAL J2000.0',
     *   'GALACTIC',
     *   'OLD GALACTIC (OHLSSON)',
     *   'OLD GALACTIC (VAN TULDER)',
     *   'SUPERGALACTIC (REVISED)'/
C                                       8 character version of CRDNAM
      DATA TABEPO/'B1950.0 ','b1900.0 ','J2000.0 ','Galactic',
     *            'Gal OHLS','Gal VanT','SuperGal'/
C-----------------------------------------------------------------------
C                                       Determine coordinate transform
C                                       for CRDTRN
      INTEPC = IROUND (EPOCH) 
C                                       Default is J 2000
      IF ((EPOCH.LE.0.5) .OR. (EPOCH.GE.7.5)) INTEPC = 3
      IF ((EPOCH.GT.1000) .AND. (EPOCH.LT.1999.5)) INTEPC = 1
C                                       If Epoch near a date
      IF (ABS(EPOCH-1900.).LT.1) INTEPC = 2
      IF (ABS(EPOCH-2000.).LT.1) INTEPC = 3
C                                       First is 1900
      IF (EPOCH.EQ.1) INTEPC = 2
C                                       Second is 1950
      IF (EPOCH.EQ.2) INTEPC = 1
C                                       Transfer name
      STREPC = CRDNAM(INTEPC)   
      TABEPC = TABEPO(INTEPC)
      LABEPC = ' '
C                                       Handle individual years.
C                                       (assume b????)
C                                       Write Year to character
      IF ((EPOCH.GT.1000) .AND. (EPOCH.LT.1999.5)) THEN
         WRITE (STREPC(13:),1000, ERR=999) EPOCH
         END IF
C                                       Find begin and end of label
      CALL TXTLEN (STREPC, I, J)
C                                       If no blanks
C                                       Use beginning to end
      IF (J.LE.0) THEN
         J = I-1
         I = 1
         END IF
C                                       Prepare to Tell user
      TABEPC = TABEPO(MIN(MAX(1,INTEPC),NCRD))
      LABEPC = 'Star positions in '//STREPC(I:J)//' Coordinates'
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (F9.4)
      END


