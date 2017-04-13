      SUBROUTINE SELINI
C-----------------------------------------------------------------------
C! Initialize data selection and control in commons in DSEL.INC
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2004, 2007, 2010, 2012
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
C   Subroutine to initialize the control values for UVGET in commons in
C   DSEL.INC.
C   Outputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME    C*12      AIPS name of input file. (blank)
C      UCLAS    C*6       AIPS class of input file. (blank)
C      UDISK    R         AIPS disk of input file. (0.0)
C      USEQ     R         AIPS sequence of input file. (0.0)
C      SOURCS   C(30)*16  Names of up to 30 sources. (blank)
C      SELQUA   I         Qualifier wanted (-1 => all)
C      SELCOD   C*4       Cal code ('    ')
C      TIMRNG   R(8)      Timerange (0s => all)
C      UVRNG    R(2)      Baseline range (0s => all)
C      STOKES   C*4       Stokes types wanted. (blank)
C      BCHAN    I         First channel number selected, (1)
C      ECHAN    I         Last channel selected. (0=>all)
C      BIF      I         First IF number selected. (1)
C      EIF      I         Last IF selected. (0=>all)
C      DOCAL    L         If true apply calibration. (false)
C      DOPOL    I         If true then correct polarization (false)
C      DOACOR   L         True if autocorrelations wanted (false)
C      DOXCOR   L         True if cross-correlations wanted (true)
C      DOWTCL   L         True if weight calibration wanted. (false)
C      DOFQSL   L         True if FREQSEL random parm present (false)
C      FRQSEL   I         Default FQ table entry to select (-1)
C      SELBAN   R         Bandwidth (Hz) to select (-1.0)
C      SELFRQ   D         Frequency (Hz) to select (-1.0)
C      DOBAND   I         >0 if bandpass calibration. (-1)
C      DOSMTH   I         if smoothing requested = 1 before, 2 after BP
C      SMOOTH   R(3)      Smoothing parameters (0.0s)
C      DXTIME   R         Integration time (days). (1 sec)
C      ANTENS   I(*)      List of antennas selected. (0=>all)
C      SUBARR   I         Subarray desired. (0=>all)
C      FGVER    I         FLAG file version number. (-100 no/no messge)
C      CLUSE    I         Cal (CL or SN) file version number (0)
C      BLVER    I         BL Table to apply (-1)
C      DOBL     L         do BL table application (false)
C      BPVER    I         BP table to apply (-1)
C      INITVS   I         Initial vis in read (1)
C      UBUFSZ   I         Size of UBUFF in AIPS bytes
C-----------------------------------------------------------------------
      INTEGER   LOOP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DPDC.INC'
C-----------------------------------------------------------------------
      UBUFSZ = UVBFSL * 2
      UNAME = ' '
      UCLAS = ' '
      UDISK = 0.0
      USEQ = 0.0
      DO 70 LOOP = 1,30
         SOURCS(LOOP) = ' '
         CALSOU(LOOP) = ' '
 70      CONTINUE
      SELQUA = -1
      SELCOD = ' '
      CALL RFILL (8, 0.0, TIMRNG)
      UVRNG(1) = 0.0
      UVRNG(2) = 0.0
      STOKES = ' '
      BCHAN = 1
      ECHAN = 0
      BIF = 1
      EIF = 0
      DOCAL = .FALSE.
      DOPOL = -1
      DOAPPL = .FALSE.
      DOWTCL = .FALSE.
      DOFQSL = .FALSE.
      FRQSEL = -1
      SELBAN = -1.0
      SELFRQ = -1.D0
      DOBAND = -1
      DOSMTH = 0
      SMOOTH(1) = 0.0
      SMOOTH(2) = 0.0
      SMOOTH(3) = 0.0
      DOACOR = .FALSE.
      DOXCOR = .TRUE.
      DXTIME = 0.0
      DORSM = .TRUE.
      CALL FILL (MAXANT, 0, ANTENS)
      SUBARR = 0
      FGVER = -100
      CLVER = 0
      CLUSE = 0
      BLVER = -1
      DOBL = .FALSE.
      BPVER = -1
      INITVS = 1
      PBPBUF = 0
      PVLBUF = 0
      PBPIBF = 0
      PDTERM = 00
      PPOLCL = 0
      PDVER = -1
      CALL DFILL (MAXIF, 0.0D0, SFREQS)
C                                       Initialize delay decorrelation
C                                       parameters to be cautious; are
C                                       properly set in UVGET('INIT')
      DO 100 LOOP = 1,MXDSVB
         DODSM(LOOP) = .FALSE.
 100     CONTINUE
      DO 120 LOOP = 1,MAXSB
         WRNVBA(LOOP) = .FALSE.
         ICQVBA(LOOP) = 0
 120     CONTINUE
C
 999  RETURN
      END
