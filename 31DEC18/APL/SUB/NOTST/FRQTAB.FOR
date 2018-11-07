      SUBROUTINE FRQTAB (DISK, CNO, LUN, CATBLK, FREQID, BUFFER, IRET)
C-----------------------------------------------------------------------
C! Fill Frequency table in common for IFs and channels
C# Modeling Header Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2011-2012
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
C   Fills in the frequency table FREQG in common /MAPDES/ (INCLUDEs
C   C/DGDS.INC) taking into account multiple IFs.  Uses values from
C   includes C/DHDR.INC and C/DUVH.INC so VHDRIN and UVPGET should have
C   been called first for the header record passed.
C   Note: uses LUN 27 if necessary to read IF table.
C   Input:
C      DISK     I        Disk number. If .le. 0 /CFILES/ scratch file
C                        number CNO will be used.
C      CNO      I        Catalog slot number
C      LUN      I        AIPS LUN to use for CHNDAT.
C      CATBLK   I(*)     Catalog header
C      FREQID   I        FREQ ID to use
C   Output:
C      BUFFER   I(512)   I/O work buffer for channel table.
C      IRET     I        Return error code, 0=>OK,
C                           5 => too many frequency channels
C                           6 => too many IFs (local dimension problem)
C                                or CHNDAT error code
C   Output in common /MAPDES/ (INCLUDEs C/DGDS.INC)
C      NCHANG   I        Number of frequency channels.
C      FREQG    D(*)     Frequency table (Hz)
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, LUN, CATBLK(256), BUFFER(512), FREQID, IRET
C
      INTEGER   NCH, NIF, ICH, IIF, VER, ITAB, FDISK, SCFCNO, IEQ
      REAL      REFPIX, REQ
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF), DRP, DFINC(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IEQ, REQ)
C-----------------------------------------------------------------------
C                                       Initialize
      NIF = 1
      FOFF(1) = 0.0D0
      VER = 1
      IF (DISK.LE.0) THEN
         FDISK = SCRVOL(CNO)
         SCFCNO = SCRCNO(CNO)
      ELSE
         FDISK = DISK
         SCFCNO = CNO
         END IF
C                                       Get IF info if available,
C                                       need now because want FINC
      CALL CHNDAT ('READ', BUFFER, FDISK, SCFCNO, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Blew local declaration of
C                                       FOFF, ISBAND, & FINC
      IF (NIF.GT.MAXIF) THEN
         IRET = 6
         WRITE (MSGTXT,1020) NIF, MAXIF
         GO TO 990
         END IF
      NCH = CATBLK(KINAX+JLOCF)
      NCHANG = NIF * NCH * CATBLK(KINAX+JLOCS)
C                                       Too many channels
      IF (NCHANG.GT.MAXCIF) THEN
         WRITE (MSGTXT,1040) NCHANG, MAXCIF
         IRET = 5
         GO TO 990
         END IF
      IEQ = CATBLK(KRCRP+JLOCF)
      REFPIX = REQ
      ITAB = 1
C                                       double precision all
      DRP = REFPIX
      DO 10 IIF = 1,NIF
         DFINC(IIF) = FINC(IIF)
 10      CONTINUE
C                                       Fill table in right order
      IF ((JLOCIF.GE.0) .AND. (JLOCIF.LT.JLOCF)) THEN
         DO 105 ICH = 1,NCH
            DO 100 IIF = 1,NIF
               FREQG(ITAB) = FREQ + (ICH-DRP)*DFINC(IIF) + FOFF(IIF)
               ITAB = ITAB + 1
 100           CONTINUE
 105        CONTINUE
      ELSE
         DO 115 IIF = 1,NIF
            DO 110 ICH = 1,NCH
               FREQG(ITAB) = FREQ + (ICH-DRP)*DFINC(IIF) + FOFF(IIF)
               ITAB = ITAB + 1
 110           CONTINUE
 115        CONTINUE
         END IF

      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRQTAB: CHNDAT ERROR ',I3)
 1020 FORMAT ('FRQTAB: TOO MANY IFS',I4,' > ',I3,' LOCALLY DEFINED')
 1040 FORMAT ('FRQTAB: TOO MANY FREQUENCY CHAN.',I4,' > ',I4)
      END
