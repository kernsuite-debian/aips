      SUBROUTINE PRENAM (NAMS, DNAMS, LSC)
C-----------------------------------------------------------------------
C! checks name-string for WaWa IO package - fills in some defaults
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
C   WaWa IO system: Prepare a NAMESTRING for use in FILOPN, FILCR,
C   FILDES, etc.   This includes checking for scratch and default use
C   Default type = 'MA', usid=0 -> NLUSER, usid=32000->any (0)
C   Inputs:
C      NAMS   C*36  Input NAMESTRING - NAME:CLASS:PHTYPE:SEQ:VOL:USID
C                       NAME,CLASS,USID are ignored if PHTYPE='SC'
C                       ASSOC will be taken as FALSE if PHTYPE='SC'
C   Outputs:
C      DNAMS  C*36      Output NAMESTRING - If PHTYPE='SC', DNAMS will
C                       contain standard scratch entries for NAME,
C                       CLASS, USID.  If PHTYPE=' ', it's set to 'MA'
C      LSC       L      T => file is a scratch file.
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER NAMS*36, DNAMS*36
      LOGICAL   LSC
C
      INTEGER   MAGIC, USEID
      INCLUDE 'INCS:DMSG.INC'
      DATA MAGIC /32000/
C-----------------------------------------------------------------------
C                                       Direct copy
      DNAMS = NAMS
C                                       Default CATTYPE
      IF (DNAMS(19:20).EQ.'  ') DNAMS(19:20) = 'MA'
C                                       Scratch file?
      LSC = DNAMS(19:20).EQ.'SC'
      IF (LSC) CALL SCRNAM (DNAMS, DNAMS)
C                                       USERID
      READ (DNAMS(30:36),1000) USEID
      IF (USEID.EQ.0) USEID = NLUSER
      IF (USEID.EQ.MAGIC) USEID = 0
      WRITE (DNAMS(30:36),1000) USEID
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I7)
      END
