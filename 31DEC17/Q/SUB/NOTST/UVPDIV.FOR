      SUBROUTINE UVPDIV (DISKI, CNOSCI, DISKO, CNOSCO, MODEL, METHOD,
     *   DOMSG, CHANEL, NCHAN, CATBLK, JBUFSZ, FREQID,  NPFLD, PCCDSK,
     *   PCCNO, PCVER, NPCC, PFLUX, BUFF1, BUFF2, BUFF3, BUFF4, IRET)
C-----------------------------------------------------------------------
C! Divides a uv data set by the Fourier transform of a polarized model.
C# AP-util UV Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000
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
C   UVPDIV divides model visibilities derived from a polarized set of
C   CLEAN components or images into the RL and LR correlations of a  uv
C   data set.  The weights of the data returned  will be the input
C   values multiplied by the model amplitude.
C      A variety of model computation methods are available; if a single
C   pass thru VISDFT, the DFT routine, is not sufficient then the data
C   is copied to a scratch file which has space for a second copy of the
C   data, the model values are computed and summed in these locations
C   and finally the model is divided into the data and written to the
C   output file.
C      Extensive use is made of commons to communicate with UVPDIV, in
C   particular /MAPDES/ (include DGDS.INC) contains most
C   of the critical information about the CLEAN components files or
C   images to be used.  Common /UVHDR/ (filled in by UVPGET) is
C   presumed to describe the uv data files.
C      Also fills in frequency table (NCHANG, FREQG) in include
C   DGDS.INC
C   Inputs:
C      DISKI      I   Input disk number. if .LE. 0 then input is a
C                     scratch file.
C      CNOSCI     I   Input file catalog slot number or /CFILES/
C                     scratch file number.
C      DISKO      I   Output disk number. if .LE. 0 then output is a
C                     scratch file.
C      CNOSCO     I   Output file catalog slot number or /CFILES/
C                     scratch file number.  If .LE. 0 then one of the
C                     internal scratch files will be used.
C      MODEL      I   1=> clean components, 2=>image.
C      METHOD     I   1=>gridded, -1=>DFT, 0=>chose.
C      DOMSG      L    If true give percent done messages for DFT.
C      CHANEL     I   First uv data channel to subtract.
C      NCHAN      I   Number of frequency channels to subtract.
C      CATBLK(256)I   Uv data catalog header record.
C      JBUFSZ     I   Size of BUFF1,2,3 in bytes, must be at least
C                     4096 words.
C      FREQID     I   Freq ID number, if it exists.
C      NPFLD      I   Number of fields in polarized model
C      PCCDSK     I(2,*) Disk numbers for Q and U models for each field
C      PCCNO      I(2,*) Catalog slot numbers for Q, U models.
C      PCVER      I(2,*) CC version numbers for for Q, U models.
C      NPCC       I(2,*) Number of CC components for for Q, U models.
C      PFLUX      R(4)   I, Q, U, V point model flux densities.
C      BUFF1,2,3,4  R   Work buffers.
C   Inputs from COMMON /MAPDES/:(DGDS.INC)
C      FACGRD      R     Value to multiply clean component fluxes
C                        by before subtraction (negative for sum).
C      SCTYPE      C*2   Scratch file type to create. (eg. 'SC')
C      NONEG       L    Stop reading comps. from a file past the first
C                       negative component. (DFT modeling ONLY)
C      LIMFLX      R    Stop at CC component < LIMFLX.
C      DOPTMD      L    Use the point model specified by PFLX, PTRAOF,
C                       PTDCOF (DFT modeling ONLY)
C      PTFLX       R    Point model flux density (Jy) (I pol. only)
C      PTRAOF      R    Point model RA offset from uv phase center
C                       (asec)
C      PTDCOF      R    Point model Dec. offset from uv phase center
C   Input from COMMON /UVHDR/:
C      LREC        I     Length of visibility record.
C      NVIS        I     Number of visibility records.
C      NRPARM      I     "Random" parameters before data, can be used
C                        to skip observed values when computing model.
C   Outputs to COMMON /MAPDES/: (DGDS.INC)
C      MFIELD      I     Number of fields
C      NSUBG(*)    I     Number of components already sub.
C      NCLNG(*)    I     Number of components per field.
C      CCDISK(*)   I     Disk numbers for CC files
C      CCCNO(*)    I     Catalog slot numbers for CC files.
C      CCVER(*)    I     CC file version number for each field.
C   Output:
C      CNOSCO     I   Output file catalog slot number or /CFILES/
C                     scratch file number.  Value returned if not
C                     specified in call.
C      IRET       I   Return error code. 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CHANEL, CNOSCI, DISKO, CNOSCO, MODEL, METHOD,
     *   NCHAN, CATBLK(256), JBUFSZ, NPFLD, PCCDSK(2,NPFLD),
     *   PCCNO(2,NPFLD), PCVER(2,NPFLD), NPCC(2,NPFLD), IRET
      LOGICAL   DOMSG
      REAL      PFLUX(4), BUFF1(*), BUFF2(*), BUFF3(*), BUFF4(*)
C
      INTEGER   IP, ISCR2, DISKX, SAVNRP, SAVLRC, LUN, LUN2, CNOX, BIF,
     *   EIF, CATSCR(256), FREQID, TEMP(256), SCFRW, IV, OV, IERR
      LOGICAL   F
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION SFOFF(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSCD.INC'
      SAVE LUN, LUN2
      DATA LUN/27/, LUN2/28/
      DATA F /.FALSE./
      DATA SFOFF /MAXIF*0.0D0/
C-----------------------------------------------------------------------
      IRET = 0
      NGRDAT = F
      SAVLRC = LREC
      SAVNRP = NRPARM
C                                       Store CATBLK for later use
      CALL COPY (256, CATBLK, SCRCAT)
      SCLREC = LREC
      SCRPRM = NRPARM
      COMPDT = CATBLK(KINAX).EQ.1
      IF (COMPDT) THEN
         CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *      WTLOC, IRET)
         IF ((IRET.NE.0) .OR. (WTLOC.LT.0)) THEN
            IRET = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IRET = 0
         END IF
C                                       Use UVMSUB for model.
C                                       Copy to padded scratch file.
 100  ISCR2 = 0
C                                       Message to about division
      MSGTXT = 'Divide data by model - first compute model by summing'
      CALL MSGWRT (3)
      CALL UVDPAD (DISKI, CNOSCI, ISCR2, JBUFSZ, BUFF1, BUFF2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get scratch file header
      IF (DISKI.LE.0) THEN
         DISKX = SCRVOL(CNOSCI)
         CNOX = SCRCNO(CNOSCI)
      ELSE
         DISKX = DISKI
         CNOX = CNOSCI
         END IF
      CALL CATIO ('READ', DISKX, CNOX, CATSCR, 'REST', BUFF1, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.4)) GO TO 999
      IRET = 0
      CALL COPY (256, CATSCR, CATBLK)
      CALL UVPGET (IERR)
C                                       Determine BIF, EIF
      BIF = 1
      EIF = 1
C                                       If more than 1 IF
      IF (JLOCIF.GT.0) THEN
         EIF = CATBLK(KINAX+JLOCIF)
C                                       Copy part portion of IF table
         IV = 1
         OV = 1
         CALL CHNCOP (IV, OV, LUN, LUN2, DISKX, SCRVOL(ISCR2), CNOX,
     *      SCRCNO(ISCR2), CATBLK, CATSCR, BIF, EIF, FREQID, SFOFF,
     *      BUFF1, BUFF2, BUFF3, BUFF4, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Compute model.
C                                       Set factor for subtraction.
      FACGRD(1) = - FACGRD(1)
C                                       Redefine record size in DUVH.INC
C                                       Input compressed
      IF (COMPDT) THEN
         NRPARM = SAVNRP + (SAVLRC - SAVNRP) * 3 - 2
         LREC = NRPARM + (SAVLRC - SAVNRP) * 3
C                                       Input NOT compresed
      ELSE
         NRPARM = SAVNRP + (SAVLRC - SAVNRP)
         LREC = NRPARM + (SAVLRC - SAVNRP)
         END IF
      DISKX = 0
C                                       Q model
      PARMOD(1) = 0.0
      KSTOK = 2
      MFIELD = NPFLD
      PTFLX = PFLUX(2)
      DO 110 IP = 1,NPFLD
         NSUBG(IP) = 1
         NCLNG(IP) = NPCC(1,IP)
         CCDISK(IP) = PCCDSK(1,IP)
         CCCNO(IP) = PCCNO(1,IP)
         CCVER(IP) = PCVER(1,IP)
 110     CONTINUE
      CALL UVMSUB (DISKX, ISCR2, DISKX, ISCR2, 0, MODEL, METHOD, CHANEL,
     *   NCHAN, F, DOMSG, CATSCR, JBUFSZ, FREQID, BUFF1, BUFF2, BUFF3,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       U model
      MFIELD = NPFLD
      KSTOK = 3
      PTFLX = PFLUX(3)
      DO 120 IP = 1,NPFLD
         NSUBG(IP) = 1
         NCLNG(IP) = NPCC(2,IP)
         CCDISK(IP) = PCCDSK(2,IP)
         CCCNO(IP) = PCCNO(2,IP)
         CCVER(IP) = PCVER(2,IP)
 120     CONTINUE
      CALL UVMSUB (DISKX, ISCR2, DISKX, ISCR2, 0, MODEL, METHOD, CHANEL,
     *   NCHAN, F, DOMSG, CATSCR, JBUFSZ, FREQID, BUFF1, BUFF2, BUFF3,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Restore DUVH.INC
      IF (COMPDT) THEN
         NRPARM = SAVNRP - 2
         LREC = NRPARM + (SAVLRC-SAVNRP) * 3
C                                       Fool UVDOUT
         SCRPRM = -LREC
         SCLREC = 0
      ELSE
         NRPARM = SAVNRP
         LREC = SAVLRC
         END IF
C                                       Output not compressed.
      COMPDT = .FALSE.
C                                       Reset factor for subtraction.
      FACGRD(1) = - FACGRD(1)
C                                       If no output file specified
C                                       use ISCR2
      IF ((DISKO.LE.0) .AND. (CNOSCO.LE.0)) CNOSCO = ISCR2
C                                       Divide/compress record to output
      CALL UVDOUT (ISCR2, DISKO, CNOSCO, CHANEL, NCHAN, JBUFSZ,
     *   BUFF1, BUFF2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Delete scratch file
      IF (ISCR2.GT.0) THEN
C                                       Save CATBLK in case it's in
C                                       common
         CALL COPY (256, CATBLK, TEMP)
         SCFRW = 2
         IV = 1
         CALL MAPCLR (IV, SCRVOL(ISCR2), SCRCNO(ISCR2), SCFRW, BUFF1)
         IF (ISCR2.EQ.NSCR) NSCR = NSCR - 1
         CALL COPY (256, TEMP, CATBLK)
         END IF
C
 999  RETURN
      END
