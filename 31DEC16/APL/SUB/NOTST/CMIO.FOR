      SUBROUTINE CMIO (OPCODE, KEYARR, IERR)
C-----------------------------------------------------------------------
C! does IO to an CM table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C  Routine to set up, or read, the arrays containing the keyword values
C  from CM tables.
C  Only called from CMINI.
C  Input:
C     OPCODE        C*4      If 'WRIT' then write keywords into KEYVAL
C                            array, if 'READ', then read them from it.
C  Input/output:
C     KEYARR(*)     I        The KEYVAL array, name different because
C                            of equivalence.
C  Input/output via common:
C     KLOCS(*)      I        Array defining location of keywords within
C                            the KEYVAL array
C     KEYTYP(*)     I        Array defining type of keyword in KEYVAL
C                            array
C     NOPOLZ        I        # polzns in the data
C     NPOLY         I        Order of polynomial used to form geometric
C                            model.
C     REVNUM        R        Revision # of correlator software used to
C                            generate correlator model.
C     OBSCODE       C*8      Observing code
C     NOSTKD        I        # polzns in the data
C     STK1          I        First Stokes parameter in the data
C     NOBAND        I        # bands (IF's) in the data.
C     NOCHAN        I        # spectral channels in the data.
C     REFFRQ        D        Freq. at reference pixel (Hz)
C     CHNBW         R        Bandwidth of single spectral channel (Hz)
C     REFPIX        R        Reference pixel
C     TABREV        I        Table revision number
C  Output:
C     IERR          I        error code: 0 => OK
C                                        1 => pointer > max size array
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      INTEGER   KEYARR(*), IERR, IPT
      CHARACTER OPCODE*4
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCMV.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       write keyword values into the
C                                       KEYVAL array to be transferred
C                                       to disc
      IF (OPCODE.EQ.'WRIT') THEN
         IPOINT = 1
         IPT = 1
C                                       Observing code
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABHOL
         CALL CHR2H (8, OBSCOD, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       # stokes in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOSTKD
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       1st Stokes in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = STK1
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       # bands (IF's) in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOBAND
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       # spectral channels in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOCHAN
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       Ref freq
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, REFFRQ, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       Chn bw
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = CHNBW
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       Ref. pixel
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = REFPIX
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       # polzns in table
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOPOLZ
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       order of polynomial
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NPOLY
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       Revision #
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = REVNUM
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWCM) GO TO 900
         IPT = IPT + 1
C                                       Table revision number
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = TABREV
         IPOINT = IPOINT + 1
C                                       fill in array to be passed
C                                       back
         CALL COPY (MAXWCM, KEYVAL, KEYARR)
C                                       read keyword values from
C                                       KEYVAL array
      ELSE IF (OPCODE.EQ.'READ') THEN
         CALL COPY (MAXWCM, KEYARR, KEYVAL)
         IPT = 1
C                                       Observing code
         IPOINT = KLOCS(IPT)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), OBSCOD)
         IPT = IPT + 1
C                                       # Stokes in data
         IPOINT = KLOCS(IPT)
         NOSTKD = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       1st Stokes in data
         IPOINT = KLOCS(IPT)
         STK1 = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       # band's (IF's) in data
         IPOINT = KLOCS(IPT)
         NOBAND = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       # spectral channels in data
         IPOINT = KLOCS(IPT)
         NOCHAN = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       Ref frq
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), REFFRQ)
         ELSE
            REFFRQ = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       Chn. bw
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            CHNBW = KEYVAD
         ELSE
            CHNBW = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       Ref. pixel
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            REFPIX = KEYVAD
         ELSE
            REFPIX = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       # polzns in table
         IPOINT = KLOCS(IPT)
         NOPOLZ = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       Order of polynomial
         IPOINT = KLOCS(IPT)
         NPOLY = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       Revision number
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            REVNUM = KEYVAD
         ELSE
            REVNUM = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       Table revision number
         IPOINT = KLOCS(IPT)
         TABREV = KEYVAL(IPOINT)
         END IF
      GO TO 999
C                                       MAXWCM error
 900  WRITE (MSGTXT,1000) IPOINT, MAXWCM
      CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CMIO: POINTER VALUE: ',I4,' > MAXIMUM: ',I4)
      END

