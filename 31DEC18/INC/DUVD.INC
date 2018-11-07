C                                                          Include DUVD.
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
C                                       Local include for UVDIF
      REAL      XID, NAME(3), CLASS(2), XSEQ, XDISK, NAME2(3),
     *   CLASS2(2), XSEQ2, XDISK2, XNCH, XBIF, XNIT, UVRANG(2), OPCODE,
     *   APARM(10), DOCRT
      REAL      BUFF(4610), BUFF2(4610), RADIUS(2), POSANG(2), FMAX(4)
      INTEGER   NITER
      LOGICAL   DOMAX, DOUVBX, ISUVR
      INTEGER   LUN, FIND, PLUN, PIND, CNO, ISTKS, NCH, BIF, SEQ, DISK,
     *   USERID, LUN2, FIND2, CNO2, SEQ2, DISK2
C                                       Local include for UVDIF
      COMMON /INPARM/ XID, NAME, CLASS, XSEQ, XDISK, NAME2, CLASS2,
     *   XSEQ2, XDISK2, XNCH, XBIF, XNIT, UVRANG, OPCODE, APARM, DOCRT
      COMMON /FNDUVC/ BUFF, BUFF2, RADIUS, POSANG, FMAX, NITER,
     *   DOMAX, DOUVBX, ISUVR, LUN, FIND, LUN2, FIND2, NCH, BIF, PLUN,
     *   PIND, CNO, CNO2, ISTKS, SEQ, DISK, SEQ2, DISK2, USERID
C                                                          End DUVD.
