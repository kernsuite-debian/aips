      SUBROUTINE SOUINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
C-----------------------------------------------------------------------
C! Create/initialize/open source (SU) table
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006, 2012, 2014-2015
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
C   Creates and initializes SOURCE extension tables or opens them for
C   reading.  SOUINI now translates the files if needed and then uses
C   SUINI to do the actual opening.  All tasks should call this
C   routine not SUINI.
C   Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   SU file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C   Input (create) / Output (pre-existing) :
C     NUMIF        I   Table keyword, gives the number of IFs
C     VELTYP       C*8 Velocity type,
C     VELDEF       C*8 Velocity defination 'RADIO','OPTICAL',
C     FREQID       I   Table keyword, denotes the FQ ID for which
C                      the SU parms have been modified. On O/P if
C                      FREQID = -999 it is not in the table, if
C                      FREQID = -1 the virgin values still exist, or
C                      the data have no FREQID random parameter.
C   Output:
C     ISURNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     SUKOLS(MAXSUC)I   The column pointer array in order, ID. NO.,
C                      SOURCE, QUAL, CALCODE, IFLUX, QFLUX, UFLUX,
C                      VFLUX, FREQO, BANDWIDTH, RAEPO, DECEPO, EPOCH,
C                      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
C                      PMRA, PMDEC
C     SUNUMV(MAXSUC)I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, VELTYP*8, VELDEF*8
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, NUMIF,
     *   FREQID, ISURNO, SUKOLS(*), SUNUMV(*), IERR
C
      INTEGER   IVER, MVER
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL FNDEXT ('SU', CATBLK, MVER)
      IVER = VER
      IF (VER.LE.0) THEN
         IVER = MVER
         IF (OPCODE.EQ.'WRIT') IVER = MVER + 1
         END IF
C                                       check format of file if read
C                                       or existing
      IF ((OPCODE.EQ.'READ') .OR. (IVER.LE.MVER)) THEN
         CALL SUREFM (DISK, CNO, IVER, CATBLK, LUN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       now do proper thing
      CALL SUINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUINI: ERROR',I3,' CORRECTING SU FORMAT, CONTINUING')
      END
