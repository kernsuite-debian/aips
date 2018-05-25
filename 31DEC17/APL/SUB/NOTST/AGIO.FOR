      SUBROUTINE AGIO (OPCODE, KEYARR, IERR)
C-----------------------------------------------------------------------
C! does IO to an AG table
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
C  from AG tables.
C  Only called from AGINI.
C
C  Input:
C     OPCODE        C*4      If 'WRIT' then write keywords into KEYVAL
C                            array, if 'READ', then read them from it.
C  Input/output:
C     KEYARR(*)     I        The KEYVAL array, name different because
C                            of equivalence.
C  Input/output in common:
C     KLOCS(*)      I        Array defining location of keywords within
C                            the KEYVAL array
C     KEYTYP(*)     I        Array defining type of keyword in KEYVAL
C                            array
C     ARRAYC(3)     D        Array center X coord. (meters, earth center)
C     SAFREQ        D        Obs. Reference Frequency for subarray(Hz)
C     RDATE         C*8      Reference date as 'DD/MM/YY'
C     ANAME         C*8      Array name
C     NUMORB        I        Number of orbital parameters
C     FRAME         C*8      Reference frame of corrdinate system
C                            (e.g. 'GEOCENTR')
C     TIMSYS        C*8      Time system, 'IAT' or 'UTC'
C     GSTIA0        D        GST at time(c.f TIMSYS) = 0hr on ref
C                            date (degrees)
C     DEGPDY        D        Earth rotation rate (deg/IAT day)
C     RDATE         C*8      Reference data as 'DD/MM/YY'
C     POLARX        R        Polar position X (metres) on ref. date
C     POLARY        R        Polar position Y (metres) on ref. date
C     UT1UTC        R        UT1 - UTC (time seconds)
C     IATUTC        R        IAT - UTC (time seconds)
C     OBSCODE       C*8      Observing code
C     NOSTKD        I        # polzns in the data
C     STK1          I        First Stokes parameter in the data
C     NOBAND        I        # bands (IF's) in the data.
C     NOCHAN        I        # spectral channels in the data.
C     REFFRQ        D        Freq. at reference pixel (Hz)
C     CHNBW         R        Bandwidth of single spectral channel (Hz)
C     REFPIX        R        Reference pixel
C     TABREV        I        Table revision number.
C  Output:
C     IERR          I        error code: 0 => OK
C                                        1 => pointer > max size array
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      INTEGER   KEYARR(*), IERR, IPT
      CHARACTER OPCODE*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAGV.INC'
      DOUBLE PRECISION KEYVAD
C-----------------------------------------------------------------------
      IERR = 0
C                                       write keyword values into the
C                                       KEYVAL array to be transferred
C                                       to disc
      IF (OPCODE.EQ.'WRIT') THEN
         IPOINT = 1
         IPT = 1
C                                       Array centre
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, ARRAYC(1), KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, ARRAYC(2), KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, ARRAYC(3), KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Subarray frequency
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, SAFREQ, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Reference date
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABHOL
         CALL CHR2H (8, RDATE, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Array name
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABHOL
         CALL CHR2H (8, ANAME, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       No. orbital parameters
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NUMORB
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Time system
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABHOL
         CALL CHR2H (8, TIMSYS, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Reference frame
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABHOL
         CALL CHR2H (8, FRAME, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       GST at IAT = 0
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, GSTIA0, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       DEGPDY
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, DEGPDY, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Polar X
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = POLARX
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Polar Y
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = POLARY
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       UT1 - UTC
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = UT1UTC
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       IAT - UTC
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = IATUTC
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Observing code
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABHOL
         CALL CHR2H (8, OBSCOD, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       # stokes in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOSTKD
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       1st Stokes in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = STK1
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       # bands (IF's) in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOBAND
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       # spectral channels in data
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOCHAN
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Ref freq
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         CALL RCOPY (NWDPDP, REFFRQ, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Chn bw
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABDBL
         KEYVAD = CHNBW
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Ref. pixel
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABFLT
         KEYVAD = REFPIX
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         IF (IPOINT.GT.MAXWAG) GO TO 900
         IPT = IPT + 1
C                                       Table revision number
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = TABREV
         IPOINT = IPOINT + 1
C                                       fill in array to be passed
C                                       back
         CALL COPY (MAXWAG, KEYVAL, KEYARR)
C                                       read keyword values from
C                                       KEYVAL array
      ELSE IF (OPCODE.EQ.'READ') THEN
         CALL COPY (MAXWAG, KEYARR, KEYVAL)
         IPT = 1
C                                       Array center
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), ARRAYC(1))
         ELSE
            ARRAYC(1) = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), ARRAYC(2))
         ELSE
            ARRAYC(2) = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), ARRAYC(3))
         ELSE
            ARRAYC(3) = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       Subarray frequency
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), SAFREQ)
         ELSE
            SAFREQ = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       Reference date
         IPOINT = KLOCS(IPT)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), RDATE)
         IPT = IPT + 1
C                                       Array name
         IPOINT = KLOCS(IPT)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), ANAME)
         IPT = IPT + 1
C                                       No. orbital parameters
         IPOINT = KLOCS(IPT)
         NUMORB = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       Reference frame
         IPOINT = KLOCS(IPT)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), FRAME)
         IPT = IPT + 1
C                                       Time system
         IPOINT = KLOCS(IPT)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), TIMSYS)
         IPT = IPT + 1
C                                       GST at IAT = 0
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), GSTIA0)
         ELSE
            GSTIA0 = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       DEGPDY
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), DEGPDY)
         ELSE
            DEGPDY = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       Polar X
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            POLARX = KEYVAD
         ELSE
            POLARX = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       Polar Y
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            POLARY = KEYVAD
         ELSE
            POLARY = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       UT1 - UTC
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            UT1UTC = KEYVAD
         ELSE
            UT1UTC = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
C                                       IAT - UTC
         IPOINT = KLOCS(IPT)
         IF (KEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            IATUTC = KEYVAD
         ELSE
            IATUTC = KEYVAR(IPOINT)
            END IF
         IPT = IPT + 1
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
C                                       Table revision number
         IPOINT = KLOCS(IPT)
         TABREV = KEYVAL(IPOINT)
         END IF
      GO TO 999
C                                       MAXWAG error
 900  WRITE (MSGTXT,1000) IPOINT, MAXWAG
      CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AGIO: POINTER VALUE: ',I4,' > MAXIMUM: ',I4)
      END
