      SUBROUTINE UVMDIV (DISKI, CNOSCI, DISKO, CNOSCO, MODEL, METHOD,
     *   DOMSG, CHANEL, NCHAN, CATBLK, JBUFSZ, FREQID, BUFF1, BUFF2,
     *   BUFF3, BUFF4, IRET)
C-----------------------------------------------------------------------
C! Divides a uv data set by the Fourier transform of a model.
C# AP-util UV Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2006, 2008
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
C   UVMDIV divides model visibilities derived from CLEAN components
C   or images into a uv data set.  The weights of the data returned
C   will be the input values multiplied by the model amplitude.
C      A variety of model computation methods are available; if a single
C   pass thru VISDFT, the DFT routine, is not sufficient then the data
C   is copied to a scratch file which has space for a second copy of the
C   data, the model values are computed and summed in these locations
C   and finally the model is divided into the data and written to the
C   output file.
C      Extensive use is made of commons to communicate with UVMDIV, in
C   particular /MAPDES/ (include DGDS.INC) contains most
C   of the critical information about the CLEAN components files or
C   images to be used.  Common /UVHDR/ (filled in by UVPGET) is
C   presumed to describe the uv data files.
C      Also fills in frequency table (NCHANG, FREQG) in include
C   DGDS.INC
C   Inputs:
C      DISKI    I        Input disk number. if <= 0 then input is a
C                        scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO    I        Output disk number. if <= 0 then output is a
C                        scratch file.
C      CNOSCO   I        Output file catalog slot number or /CFILES/
C                        scratch file number.  If <= 0 then one of the
C                        internal scratch files will be used.
C      MODEL    I        1=> clean components, 2=>image.
C      METHOD   I        1=>gridded, -1=>DFT, 0=>chose.
C      DOMSG    L        If true give percent done messages for DFT.
C      CHANEL   I        First uv data channel to subtract.
C      NCHAN    I        Number of frequency channels to subtract.
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
C      FACGRD   R(2)     Value to multiply clean component fluxes by
C                        before subtraction (negative for sum).
C                        FACGRD(2) is for data and 0 or 1 only values
C                        used.  Model added not subtracted when data are
C                        ignored.
C      SCTYPE   C*2      Scratch file type to create. (eg. 'SC')
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop when flux < LIMFLX.
C      DOPTMD   L        Use the point model specified by PTFLX, PTRAOF,
C                        PTDCOF (DFT modeling ONLY)
C      PTFLX    R        Point model flux density (Jy) (I pol. only)
C      PTRAOF   R        Point model RA offset from uv phase center
C                        (asec)
C      PTDCOF   R        Point model Dec. offset from uv phase center
C   Input from COMMON /UVHDR/:
C      LREC     I        Length of visibility record.
C      NVIS     I        Number of visibility records.
C      NRPARM   I        "Random" parameters before data, can be used
C                        to skip observed values when computing model.
C   Output:
C      CNOSCO   I        Output file catalog slot number or /CFILES/
C                        scratch file number.  Value returned if not
C                        specified in call.
C      BUFF1    R(*)     Work buffers.
C      BUFF2    R(*)     Work buffers.
C      BUFF3    R(*)     Work buffers.
C      BUFF4    R(*)     Work buffers.
C      IRET     I        Return error code. 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, MODEL, METHOD, CHANEL,
     *   NCHAN, CATBLK(256), JBUFSZ, FREQID, IRET
      LOGICAL   DOMSG
      REAL      BUFF1(*), BUFF2(*), BUFF3(*), BUFF4(*)
C
      INTEGER   I, LENMOD, ISCR2, DISKX, SAVNRP, SAVLRC, DISK, CNO, LUN,
     *   LUN2, INMETH, OUMETH, CNOX, BIF, EIF, CATSCR(256), TEMP(256),
     *   LENBU, MXCMP, XNCC, APSIZ, SCFRW, IV, OV, NEED, MSGSAV, KAP,
     *   LCHANL, LCHAN
      LOGICAL   DOSUM, DODFT, F, LTEMP
      REAL      BUFSZ, TIMDFT, TIMFFT
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION SFOFF(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:DMOD.INC'
      SAVE LUN, LUN2
      DATA LENMOD /7/, LUN/27/, LUN2/28/
      DATA F /.FALSE./
      DATA SFOFF /MAXIF*0.0D0/
C-----------------------------------------------------------------------
      IRET = 0
      NGRDAT = F
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
C                                       Decide model computation method.
      INMETH = METHOD
      DODFT = (METHOD.LT.0) .AND. (MODEL.EQ.1)
C                                       DFT only for pt. model
      CALL UVMTYP (0, METHOD, NCHAN, LTEMP, TIMDFT, TIMFFT)
      IF (MODEL.EQ.1) DODFT = LTEMP
      DODFT = DODFT .OR. DOPTMD
      OUMETH = 1
      IF (DODFT) OUMETH = -1
C                                       If doing Direct Fourier Trans.
      IF (DODFT) THEN
C                                       Check point model
         IF (DOPTMD) THEN
            XNCC = 1
         ELSE
            XNCC = 0
            DO 20 I = 1,MFIELD
               XNCC = XNCC + NCLNG(I) - NSUBG(I)
 20            CONTINUE
            END IF
C                                       Make AP memory for this
         BUFSZ = JBUFSZ
         LENBU = ((BUFSZ-2*NBPS) / 2) / (LREC*2)
         NEED = 12 + (LENBU*LREC) + CATBLK(KINAX+JLOCF) + XNCC*LENMOD
     *      + 2*LENMOD
         NEED = NEED / 1024
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL QINIT (NEED, 0, KAP)
         MSGSUP = MSGSAV
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = 12 + (LENBU*LREC) + CATBLK(KINAX+JLOCF) +
     *         XNCC*LENMOD/10 + 2*LENMOD
            NEED = NEED / 1024
            NEED = MIN (32+1024, NEED) + 2
            CALL QINIT (NEED, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               IRET = 8
               MSGTXT = 'UVMDIV CANNOT GET NEEDED MEMORY'
               CALL MSGWRT (8)
               END IF
            END IF
         APSIZ = PSAPNW * 1024
         CALL QRLSE
C                                       Decide if will fit.in AP
         MXCMP = (APSIZ-10.-(LENBU*LREC)-2.-CATBLK(KINAX+JLOCF))
     *    / LENMOD
         MXCMP = MXCMP - 2
C                                       If room for all components
         IF (XNCC.LE.MXCMP) THEN
C                                       IF using a scratch file
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
C                                       Divide model.
            CALL VISDFT ('DIV ', CHANEL, NCHAN, DISKI, CNOSCI, DISKO,
     *         CNOSCO, 0, DOSUM, DOMSG, CATBLK, JBUFSZ, BUFF1, BUFF2,
     *         BUFF3, IRET)
C                                       Check for too many comps.
            IF (IRET.EQ.10) GO TO 100
            GO TO 999
            END IF
         END IF
C                                       Use UVMSUB for model.
C                                       Copy to padded scratch file.

 100  ISCR2 = 0
C                                       Message to about division
      MSGTXT = 'Divide data by model - first compute model by summing'
      IF (MSGSUP.NE.32000) CALL MSGWRT (3)
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
C                                       End if more than 1 IF
         END IF
C                                       Compute model.
C                                       Set factor for subtraction.
      FACGRD(1) = - FACGRD(1)
C                                       Redefine record size in
C                                       /UVHDR/.
      DISKX = 0
      SAVNRP = NRPARM
      SAVLRC = LREC
      NRPARM = LREC
      LREC = 2 * SAVLRC - SAVNRP
      CALL UVMSUB (DISKX, ISCR2, DISKX, ISCR2, 0, MODEL, METHOD, CHANEL,
     *   NCHAN, F, DOMSG, CATBLK, JBUFSZ, FREQID, BUFF1, BUFF2, BUFF3,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Restore /UVHDR/
      NRPARM = SAVNRP
      LREC = SAVLRC
C                                       Reset factor for subtraction.
      FACGRD(1) = - FACGRD(1)
C                                       If no output file specified
C                                       use ISCR2
      IF ((DISKO.LE.0) .AND. (CNOSCO.LE.0)) CNOSCO = ISCR2
C                                       Divide/compress record to output
      LCHANL = CHANEL
      LCHAN = NCHAN
      IF (MODMAX.GT.0) THEN
         LCHAN = 0
         LCHANL = 1000000
         DO 110 I = 1,MODMAX
            IV = MODCHN(I) + MODNCH(I) - 1
            LCHAN = MAX (LCHAN, IV)
            LCHANL = MIN (LCHANL, MODCHN(I))
 110        CONTINUE
         END IF
      CALL UVDOUT (ISCR2, DISKO, CNOSCO, LCHANL, LCHAN, JBUFSZ,
     *   BUFF1, BUFF2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Delete scratch file
      IF (ISCR2.GT.0) THEN
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
