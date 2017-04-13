      SUBROUTINE FSWTCH (FIL1, FIL2, VOL1, VOL2, CNO1, CNO2, BO1, BO2)
C-----------------------------------------------------------------------
C! switches names and addresses of two files
C# IO-util
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
C   FSWTCH switches the names and addresses of two files.
C   In/out:
C      FIL1  FIL2   C*48   Names of the two files.
C      VOL1  VOL2   I      Volume numbers of the two files.
C      CNO1  CNO2   I      Catalog numbers of the two files
C      BO1   BO2    I      Block offsets of the two files.
C-----------------------------------------------------------------------
      CHARACTER FIL1*48, FIL2*48
      INTEGER   VOL1, VOL2, CNO1, CNO2, BO1, BO2
C
      CHARACTER CTEMP*48
      INTEGER   ITEMP
C-----------------------------------------------------------------------
C                                       Switch volume numbers.
      ITEMP = VOL1
      VOL1 = VOL2
      VOL2 = ITEMP
C                                       Switch catalog numbers.
      ITEMP = CNO1
      CNO1 = CNO2
      CNO2 = ITEMP
C                                       Switch block offsets.
      ITEMP = BO1
      BO1 = BO2
      BO2 = ITEMP
C                                       Switch file names.
      CTEMP = FIL1
      FIL1 = FIL2
      FIL2 = CTEMP
C
 999  RETURN
      END
