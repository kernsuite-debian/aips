      SUBROUTINE UVMSUB (DISKI, CNOSCI, DISKO, CNOSCO, IFIELD, MODEL,
     *   METHOD, CHANEL, NCHAN, DOSUM, DOMSG, CATBLK, JBUFSZ, FREQID,
     *   BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Subtracts the Fourier transform of a model from a uv data set.
C# AP-util UV Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2008-2009
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
C   UVMSUB subtracts a clean model or an image from a set of uv data.
C   Extensive use is made of commons to communicate with UVMSUB, in
C   particular /MAPDES/ (include DGDS.INC) contains most
C   of the critical information about the CLEAN components files or
C   images to be subtracted.  Common /UVHDR/ (filled in by UVPGET) is
C   presumed to describe the uv data files.
C      Also fills in frequency table (NCHANG, FREQG) in includes
C   DGDS.INC
C   Inputs:
C      DISKI    I        Input disk number. if <= 0 then input is a
C                        scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO    I        Output disk number. if <= 0 then output is a
C                        scratch file.
C      CNOSCO   I        Output file catalog slot number or /CFILES/
C                        scratch file number.
C      IFIELD   I        If CC, field number (0 -> all)
C      MODEL    I        1=> clean components, 2=>image.
C      METHOD   I        1=>gridded, -1=>DFT, 0=>chose.
C      CHANEL   I        First uv data channel to subtract.
C      NCHAN    I        Number of frequency channels to subtract.
C      DOSUM    L        If true then sum component fluxes in FLUXG,
C                        TFLUXG.
C      DOMSG    L        If true give percent done messages for DFT.
C      CATBLK   I(256)   UV data catalog header record.
C      JBUFSZ   I        Size of BUFF1,2,3 in bytes, must be at least
C                        4096 words.
C      FREQID   I        Freq ID number, if it exists.
C   Inputs from COMMON /MAPDES/:
C      MFIELD   I        Number of fields
C      NSUBG    I(*)     Number of components already sub.
C      NCLNG    I(*)     Number of components per field.
C      CCDISK   I(*)     Disk numbers for CC files
C      CCCNO    I(*)     Catalog slot numbers for CC files.
C      CCVER    I(*)     CC file version number for each field.
C      FACGRD   R(2)     Value to multiply clean component fluxes
C                        by before subtraction (negative for sum).
C                        FACGRD(2) is for data and 0 or 1 only values
C                        used.  Model added not subtracted when data are
C                        ignored.
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop reading comps < LIMFLX in abs value
C      DOPTMD   L        Use the point model specified by PTFLX, PTRAOF,
C                        PTDCOF (DFT modeling ONLY)
C      PTFLX    R        Point model flux density (Jy) (I pol. only)
C      PTRAOF   R        Point model RA offset from uv phase center
C                        (arcsec)
C      PTDCOF   R        Point model Dec. offset from uv phase center
C   Input from COMMON /UVHDR/:
C      LREC     I        Length of visibility record.
C      NVIS     I        Number of visibility records.
C      NRPARM   I        "Random" parameters before data, can be used
C                        to skip observed values when computing model.
C   Output:
C      BUFF1    R(*)     Work buffers.
C      BUFF2    R(*)     Work buffers.
C      BUFF3    R(*)     Work buffers.
C      IRET     I        Return error code. 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, IFIELD, MODEL, METHOD,
     *   CHANEL, NCHAN, CATBLK(256), JBUFSZ, IRET
      LOGICAL   DOSUM, DOMSG
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
C
      INTEGER   SCRGRD, SCRWRK, DISK, CNO, LUN, INMETH, OUMETH, FREQID
      LOGICAL   DODFT, LTEMP
      REAL      TIMDFT, TIMFFT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      SAVE LUN, SCRGRD, SCRWRK
      DATA SCRGRD, SCRWRK /0, 0/, LUN /27/
C-----------------------------------------------------------------------
      IRET = 0
      NGRDAT = .FALSE.
C                                       Store CATBLK for later use
      IF (.NOT.DATDIV) THEN
         CALL COPY (256, CATBLK, SCRCAT)
         SCLREC = LREC
         SCRPRM = NRPARM
         COMPDT = CATBLK(KINAX).EQ.1
         IF (COMPDT) THEN
            CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *         WTLOC, IRET)
            IF ((IRET.NE.0) .OR. (WTLOC.LT.0)) THEN
               IRET = 5
               MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED'
     *            // ' DATA'
               CALL MSGWRT (8)
               GO TO 999
               END IF
            IRET = 0
            END IF
         END IF
C                                       If using a scratch file
      IF (DISKI.LE.0) THEN
         DISK = SCRVOL(CNOSCI)
         CNO = SCRCNO(CNOSCI)
      ELSE
         DISK = DISKI
         CNO = CNOSCI
         END IF
C                                       Fill Frequency table.
      CALL FRQTAB (DISK, CNO, LUN, CATBLK, FREQID, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Decide model computation method.
      INMETH = METHOD
      DODFT = (METHOD.LT.0) .AND. (MODEL.EQ.1)
C                                       DFT only for pt. model
      CALL UVMTYP (IFIELD, METHOD, NCHAN, LTEMP, TIMDFT, TIMFFT)
      IF (MODEL.EQ.1) DODFT = LTEMP
      DODFT = DODFT .OR. DOPTMD
      OUMETH = 1
      IF (DODFT) OUMETH = -1
C                                       If doing DFT model subtraction
      IF (DODFT) THEN
         CALL VISDFT ('SUB ', CHANEL, NCHAN, DISKI, CNOSCI, DISKO,
     *      CNOSCO, IFIELD, DOSUM, DOMSG, CATBLK, JBUFSZ, BUFF1, BUFF2,
     *      BUFF3, IRET)
C                                       Gridded interpolation method.
      ELSE
         DOFFT = (MODEL.EQ.2) .OR. (MODEL.EQ.3)
         CALL GRDSUB (MODEL, DOSUM, SCRGRD, SCRWRK, DISKI, CNOSCI,
     *      DISKO, CNOSCO, IFIELD, CHANEL, NCHAN, CATBLK, JBUFSZ, BUFF1,
     *      BUFF2, BUFF3, IRET)
         END IF
C
 999  RETURN
      END
