#include <R.h>
#include <Rmath.h>
#include "ursa.h"

#define memoverlap1 memcpy
#define memoverlap memmove

int progressBar(int cur,int max,char *text)
{
   if (!cur)
   {
      if (strlen(text))
         Rprintf("%s 0.",text);
      else
         Rprintf("0.");
      return(0);
   }
   if ((!cur)||(cur+1>max))
      return(0);
   if ((cur+1)>=max)
   {
      Rprintf(" Done!\n");
      return(0);
   }
   int mark1=(short)(cur*40/max);
   int mark2=(short)((cur+1)*40/max);
   int mark3=(short)(mark2/4);
   int mark4=(short)((mark2+1)/4);
   if (mark1==mark2)
      return(0);
   if (mark3==mark4)
      Rprintf(".");
   else
      Rprintf("%d",mark4*10);
   return(0);
}
void makemap4(double *x,double *_bg,int *dim,double *_cover,double *weight
             ,int *_sum,double *res)
{
   int samples=dim[0];
   int bands=dim[1];
   int c,t,adr,adrAvg;
   double Mx,w;
   int n;
   int sum=*_sum;
   double cover=*_cover;
   double background=*_bg;
   double eps=1e-38;
  // Rprintf("dim=c(%d,%d) cover=%f bg=%f\n",samples,bands,cover,background);
   if (0)
   {
      for (t=0;t<bands;t++)
         Rprintf(" %.2f",weight[t]);
      Rprintf("\n");
   }
   for (c=0;c<samples;c++)
   {
      Mx=w=0.0;
      n=0;
      adrAvg=0*samples+c;
      adr=c;
      for (t=0;t<bands;t++,adr+=samples)
      {
        // printf(" %5.2f(%2d)",x[adr],adr);
         if (fabs(x[adr]-background)<eps) // if (ISNA(x[adr])) {
            continue;
         Mx+=x[adr]*weight[t];
         if (!sum)
            w+=weight[t];
         n++;
      }
     // Rprintf(" %d",n);
      if (sum)
         w=1.0;
      if ((float)n/(float)bands<cover)
         res[adrAvg]=background; // NA_REAL;
      else
         res[adrAvg]=Mx/w;// '/(float)n';
     // printf(" res[%2d]=%5.2f *** %5.2f ***\n",adr2,res[adr2],Mx);
   }
  // Rprintf("\n");
   return;
}
void focalMean(double *x,double *bg,int *dim,double *W,double *cvr,int *Z,int *E
            ,int *verbose,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   float winsize=(float)*W;
   float cover=(float)*cvr;
   short fillNA=(short)*Z;
   short saveMargin=*E;
   short i,size;
   size=(short)floor(winsize);
   if (!(size%2))
      size--;
   short shift=(short)(size/2);
   float koeff=(winsize-size)/2.0;
  // size=(short)floor(winsize);
  // if (!(size%2))
  //    size--;
   koeff=(winsize-size)/2.0;
   size=(short)ceil(winsize);
   if (!(size%2))
      size++;
   shift=(short)(size/2);
   int c,r,t,k,adr,adr1,adr2,c2,r2;
  // float *work;
   double *valuein=(double *)malloc(samples*size*sizeof(double));
   double *empty=(double *)malloc(samples*1*sizeof(double));
   double Mout,*M=(double *)malloc(samples*1*sizeof(double));
   float nout,*n=(float *)malloc(samples*1*sizeof(float));
   double background=*bg; //NA_REAL
   float sizex,sizey;
   double eps=1e-38;
   if (*verbose)
      Rprintf("fspatial: r=%d c=%d b=%d koeff=%.2f size=%d shift=%d winsize=%.1f"
             " fillNA=%d bg=%.1f\n"
            ,lines,samples,bands,koeff,size,shift,winsize,fillNA,background);
   for (adr=0;adr<samples*1;adr++)
       empty[adr]=background;
   sizex=sizey=winsize;
   for (t=0;t<bands;t++) // get band
   {
      adr1=t*lines*samples;
     // printf("Band %d:\n",t+1);
      for (adr=0;adr<samples*size;adr++)
          valuein[adr]=background;
      for (r=0;r<shift+lines;r++)
      {
         memset(M,0,samples*sizeof(double));
         memset(n,0,samples*sizeof(float));
         memoverlap(valuein,valuein+samples,(size-1)*samples*sizeof(double));
         if (r<lines)
            memcpy(valuein+(size-1)*samples,x+adr1+r*samples,samples*sizeof(double));
         else
            memcpy(valuein+(size-1)*samples,empty,samples*sizeof(double));
         Mout=nout=0.0;
         for (k=0;k<size*samples;k++)
         {
            if (fabs(valuein[k]-background)<eps)
               continue;
            Mout+=valuein[k];
            nout+=1.0;
         }
        // fprintf(stdout,"r=%3d M=%f n=%f\n",r,Mout,nout);
         if (r<shift)
            continue;
         for (c=0;c<samples;c++)
         {
            adr2=adr1+r*samples+c;
           // printf(" a2[%2d]=%4.1f",adr2,x[adr2]);
            for (i=0;i<size;i++)
            {
               if (fabs(valuein[i*samples+c]-background)<eps)
                  continue;
               if ((koeff>0.0)&&((i==0)||(i==size-1)))
               {
                  n[c]+=koeff;
                  M[c]+=koeff*valuein[i*samples+c];
               }
               else //
               {
                  n[c]+=1.0;
                  M[c]+=valuein[i*samples+c];
               }
            }
         }
         for (c=0;c<samples;c++)
         {
            r2=r-shift;
            c2=c;
            adr2=adr1+r2*samples+c2;
            if (saveMargin) {
               if (r2<shift)
                  sizey=shift;
               else if (lines-r2-1<shift)
                  sizey=lines-r2-1;
               else
                  sizey=winsize;
               if (c2<shift)
                  sizex=shift;
               else if (samples-c2-1<shift)
                  sizex=samples-c2-1;
               else
                  sizex=winsize;
            }
            Mout=0.0;
            nout=0.0;
            for (i=-shift;i<=shift;i++)
            {
               if (c+i<0 || c+i>=samples)
                  continue;
               if ((koeff>0.0)&&((i==-shift)||(i==shift)))
               {
                  Mout+=M[i+c]*koeff;
                  nout+=n[i+c]*koeff;
               }
               else
               {
                  Mout+=M[i+c];
                  nout+=n[i+c];
               }
            }
           // printf(" r2[%2d]=%5.2f/%.1f^%.2f",adr2,Mout,nout
           //       ,nout/(winsize*winsize*cover));
           // continue;
            if ((nout>0.0)&&(nout>=sizex*sizey*cover))
            {
               if ((0)||(!fillNA))
                  res[adr2]=Mout/nout;
               else
               {
                  if ((fabs(x[adr2]-background)<eps)||(!fillNA))
                     res[adr2]=Mout/nout;
                  else
                     res[adr2]=x[adr2];
               }
            }
            else
            {
              // Iout.line[c]=background;  //не было до 11 ноября 2003 г.
              // Iout.line[c]=Iin.line[c]; //глюки случились 16 декабря 2003 г.
               if (nout>0.0)
                  res[adr2]=background;
               else
                  res[adr2]=x[adr2];
            }
         }
      }
      //~ Rprintf("\n");
   }
   for (adr=0;adr<(samples*lines*bands);adr++)
   {
     // printf(" %4.1f",res[adr]);
     // res[adr]=x[adr];
   }
  // printf("\n");

   
   free(valuein);
   free(empty);
   free(M);
   free(n);
   
   valuein=NULL;
   empty=NULL;
   M=NULL;
   n=NULL;
   
   return;
}
void focalMeanWithNA(double *x,int *dim,double *W,double *cvr,int *Z,int *verbose
                  ,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   float winsize=(float)*W;
   float cover=(float)*cvr;
   short fillNA=(short)*Z;
   short i,size=(short)floor(winsize);
   if (!(size%2))
      size--;
   short shift=(short)(size/2);
   float koeff=(winsize-size)/2.0;
   size=(short)floor(winsize);
   if (!(size%2))
      size--;
   koeff=(winsize-size)/2.0;
   size=(short)ceil(winsize);
   if (!(size%2))
      size++;
   shift=(short)(size/2);
   int c,r,t,k,adr,adr1,adr2,c2,r2;
  // float *work;
   double *valuein=(double *)malloc(samples*size*sizeof(double));
   double *empty=(double *)malloc(samples*1*sizeof(double));
   double Mout,*M=(double *)malloc(samples*1*sizeof(double));
   float nout,*n=(float *)malloc(samples*1*sizeof(float));
   float sizex,sizey;
   if (*verbose)
      Rprintf("focalMeanWithNA: r=%d c=%d b=%d koeff=%.2f size=%d shift=%d winsize=%.1f\n"
            ,lines,samples,bands,koeff,size,shift,winsize);
   for (adr=0;adr<samples*1;adr++)
       empty[adr]=NA_REAL;
   for (t=0;t<bands;t++) // get band
   {
      adr1=t*lines*samples;
     // printf("Band %d:\n",t+1);
      for (adr=0;adr<samples*size;adr++)
          valuein[adr]=NA_REAL;
      for (r=0;r<shift+lines;r++)
      {
         memset(M,0,samples*sizeof(double));
         memset(n,0,samples*sizeof(float));
         memoverlap(valuein,valuein+samples,(size-1)*samples*sizeof(double));
         if (r<lines)
            memcpy(valuein+(size-1)*samples,x+adr1+r*samples,samples*sizeof(double));
         else
            memcpy(valuein+(size-1)*samples,empty,samples*sizeof(double));
         Mout=nout=0.0;
         for (k=0;k<size*samples;k++)
         {
            if (ISNA(valuein[k]))
               continue;
            Mout+=valuein[k];
            nout+=1.0;
         }
        // fprintf(stdout,"r=%3d M=%f n=%f\n",r,Mout,nout);
         if (r<shift)
            continue;
         for (c=0;c<samples;c++)
         {
            adr2=adr1+r*samples+c;
           // printf(" a2[%2d]=%4.1f",adr2,x[adr2]);
            for (i=0;i<size;i++)
            {
               if (ISNA(valuein[i*samples+c]))
                  continue;
               if ((koeff>0.0)&&((i==0)||(i==size-1)))
               {
                  n[c]+=koeff;
                  M[c]+=koeff*valuein[i*samples+c];
               }
               else //
               {
                  n[c]+=1.0;
                  M[c]+=valuein[i*samples+c];
               }
            }
         }
         for (c=0;c<samples;c++)
         {
            r2=r-shift;
            c2=c;
            adr2=adr1+r2*samples+c2;
            if (r2<shift)
               sizey=shift;
            else if (lines-r2-1<shift)
               sizey=lines-r2-1;
            else
               sizey=winsize;
            if (c2<shift)
               sizex=shift;
            else if (samples-c2-1<shift)
               sizex=samples-c2-1;
            else
               sizex=winsize;
            Mout=0.0;
            nout=0.0;
            for (i=-shift;i<=shift;i++)
            {
               if (c+i<0 || c+i>=samples)
                  continue;
               if ((koeff>0.0)&&((i==-shift)||(i==shift)))
               {
                  Mout+=M[i+c]*koeff;
                  nout+=n[i+c]*koeff;
               }
               else
               {
                  Mout+=M[i+c];
                  nout+=n[i+c];
               }
            }
           // printf(" r2[%2d]=%5.2f/%.1f^%.2f",adr2,Mout,nout
           //       ,nout/(winsize*winsize*cover));
           // continue;
            if ((nout>0.0)&&(nout>=sizex*sizey*cover))
            {
               if ((0)||(!fillNA))
                  res[adr2]=Mout/nout;
               else
               {
                  if ((ISNA(x[adr2]))||(!fillNA))
                     res[adr2]=Mout/nout;
                  else
                     res[adr2]=x[adr2];
               }
            }
            else
            {
              // Iout.line[c]=background;  //не было до 11 ноября 2003 г.
              // Iout.line[c]=Iin.line[c]; //глюки случились 16 декабря 2003 г.
               if (nout>0.0)
                  res[adr2]=NA_REAL;
               else
                  res[adr2]=x[adr2];
            }
         }
        //   printf("\n");
      }
   }
  // for (adr=0;adr<(samples*lines*bands);adr++)
  // {
     // printf(" %4.1f",res[adr]);
     // res[adr]=x[adr];
  // }
  // printf("\n");
   return;
}
void focalExtrem(double *x,int *K,double *bg,int *dim,int *S,double *cvr,int *Z
             ,int *E,int *verb,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   float cover=(float)*cvr;
   short fillNA=(short)*Z;
   short saveMargin=(short)*E;
   short i;
   short size=*S;
   float kind;
   if (*K<0)
      kind=-1.0;
   else
      kind=1.0;
   if (!(size%2))
      size++;
   short shift=(short)(size/2);
   shift=(short)(size/2);
   int c,r,t,adr,adr1,adr2,c2,r2;
  // float *work;
   double *valuein=(double *)malloc(samples*size*sizeof(double));
   double *empty=(double *)malloc(samples*1*sizeof(double));
   double Mout,*M=(double *)malloc(samples*1*sizeof(double));
   float nout,*n=(float *)malloc(samples*1*sizeof(float));
   double background=*bg; //NA_REAL
   float sizex,sizey;
   double eps=1e-38;
   int verbose=*verb;
   if (verbose)
      Rprintf("focalExtrem: r=%d c=%d b=%d size=%d shift=%d fillNA=%d bg=%.1f\n"
            ,lines,samples,bands,size,shift,fillNA,background);
   for (adr=0;adr<samples*1;adr++)
       empty[adr]=background;
   sizex=sizey=size;
   for (t=0;t<bands;t++) // get band
   {
      adr1=t*lines*samples;
     // printf("Band %d:\n",t+1);
      for (adr=0;adr<samples*size;adr++)
          valuein[adr]=background;
      for (r=0;r<shift+lines;r++)
      {
         memoverlap(valuein,valuein+samples,(size-1)*samples*sizeof(double));
         if (r<lines)
            memcpy(valuein+(size-1)*samples,x+adr1+r*samples,samples*sizeof(double));
         else
            memcpy(valuein+(size-1)*samples,empty,samples*sizeof(double));
         if (r<shift)
            continue;
         memset(n,0,samples*sizeof(float));
         for (c=0;c<samples;c++)
         {
            adr2=adr1+r*samples+c;
            //~ if (kind==1)
               //~ M[c]=1e38;
            //~ else
               //~ M[c]=-1e38;
            M[c]=-kind*1e38;
            for (i=0;i<size;i++)
            {
               if (fabs(valuein[i*samples+c]-background)<eps)
                  continue;
               n[c]+=1.0;
               if ((valuein[i*samples+c]-M[c])*kind>0)
                  M[c]=valuein[i*samples+c];
               //~ if (kind==1)
                  //~ if (valuein[i*samples+c]<M[c])
                     //~ M[c]=valuein[i*samples+c];
               //~ else
                  //~ if (valuein[i*samples+c]>M[c])
                     //~ M[c]=valuein[i*samples+c];
            }
         }
         for (c=0;c<samples;c++)
         {
            r2=r-shift;
            c2=c;
            adr2=adr1+r2*samples+c2;
            if (saveMargin) {
               if (r2<shift)
                  sizey=shift;
               else if (lines-r2-1<shift)
                  sizey=lines-r2-1;
               else
                  sizey=size;
               if (c2<shift)
                  sizex=shift;
               else if (samples-c2-1<shift)
                  sizex=samples-c2-1;
               else
                  sizex=size;
            }
            Mout=-kind*1e38;
            nout=0.0;
            for (i=-shift;i<=shift;i++)
            {
               if (c+i<0 || c+i>=samples)
                  continue;
               nout+=n[i+c];
               if ((M[i+c]-Mout)*kind>0) 
                  Mout=M[i+c];
            }
            if ((nout>0.0)&&(nout>=sizex*sizey*cover))
            {
               if ((0)||(!fillNA))
                  res[adr2]=Mout;
               else
               {
                  if ((fabs(x[adr2]-background)<eps)||(!fillNA))
                     res[adr2]=Mout;
                  else
                     res[adr2]=x[adr2];
               }
            }
            else
            {
               if (nout>0.0)
                  res[adr2]=background;
               else
                  res[adr2]=x[adr2];
            }
         }
      }
   }
   
   free(valuein);
   free(empty);
   free(M);
   free(n);
   
   valuein=NULL;
   empty=NULL;
   M=NULL;
   n=NULL;
   
   return;
}
void makeField(double *x,int *dim,int *res)
{
   int t=dim[1];
   int n=dim[0];
   int i,j,adr,k=0;
  // printf("t=%d n=%d\n",t,n);
  // memset(res,0,n*sizeof(int));
   for (j=0;j<n;j++)
   {
      for (i=0;i<t;i++)
      {
         adr=i*n+j;
         if (ISNA(x[adr]))
            continue;
         res[j]=j+1;
         k++;
         break;
      }
   }
   if ((1)&&(2*k>n))
   {
      for (j=0;j<n;j++)
         if (!res[j])
            res[j]=-j-1;
         else
            res[j]=0;
   }
   return;
}

void optimalDatatypeInt(int *x,int *n,int *res)
{
   short val[32];
   int i,len=*n;
  // printf("len=%d\n",len);
  // for (i=0;i<32;i++)
   memset(val,0,sizeof(short)*32);
   for (i=0;i<len;i++)
   {
     // printf("x[%d]=%d\n",i,x[i]);
      if (x[i]==-0x80000000)
         continue;
      if (((!val[2])||(!val[12]))&&(x[i]>=0)&&(x[i]<=255))
      {
         if (!val[1])
            val[1]=1;
      }
      else if ((x[i]>=-32768)&&(x[i]<=32767)) //&&(!val[2])
      {
         if (!val[2])
            val[2]=2;
      }
      else if ((x[i]>=0)&&(x[i]<=65535)) //&&(!val[12])
      {
         if (!val[12])
            val[12]=12;
      }
      else if ((!val[3])&&(!((val[1])||(val[2])||(val[12]))))
      {
        // printf("x[%d]=%d\n",i,x[i]);
         val[3]=3;
         break;
      }
   }
   if (0)
   {
      for (i=0;i<32;i++)
         Rprintf(" %d(%d)",val[i],i);
      Rprintf("\n");
   }
   if ((val[12])&&(val[2]))
      val[3]=3;
   if (val[4])
      *res=4;
   if (val[3])
      *res=3;
   else if (val[2])
      *res=2;
   else if (val[12])
      *res=12;
   else if (val[1])
      *res=1;
   return;
}
void optimalDatatypeDouble(double *x,int *n,int *res)
{
   short val[32];
   int i,len=*n;
   memset(val,0,sizeof(short)*32);
   double eps,eps2;
   double pos=(double)2147483647;
   double neg=(double)(-2147483647);
   for (i=0;i<len;i++)
   {
      if (ISNA(x[i]))
         continue;
      eps=fabs(x[i]-floor(x[i])); // 'floor' is quick, 'round' is robust
     // printf(" %f",eps);
      if (eps>1e-11)
      {
         eps2=fabs(eps-1);
        // Rprintf("x=%f(%f) eps=%f(%f)\n",x[i],floor(x[i]),eps,eps2);
         if (eps2>1e-11) {
            val[4]=4;
            break;
         }
      }
      else if ((!((val[2])||(val[12])||(val[3])))&&(x[i]>=0)&&(x[i]<=255))
         val[1]=1;
      else if ((!val[3])&&(x[i]>=-32768)&&(x[i]<=32767))
         val[2]=2;
      else if ((!(val[3]))&&(x[i]>=0)&&(x[i]<=65535))
         val[12]=12;
     // else if ((!val[3])&&(!((val[1])||(val[2])||(val[12]))))
     //    val[3]=3;
      else if ((x[i]>=neg)&&(x[i]<=pos))
         val[3]=3;
      else if ((x[i]<neg)||(x[i]>pos)) {
         val[4]=4;
         break;
      }
   }
   if (0)
   {
      Rprintf("bg=%f\n",x[len-1]);
      for (i=0;i<32;i++)
         Rprintf(" %d(%d)",val[i],i);
      Rprintf("\n");
   }
   if (val[4])
      *res=4;
   else if ((val[12])&&(val[2]))
      val[3]=3;
   else if (val[3])
      *res=3;
   else if (val[2])
      *res=2;
   else if (val[12])
      *res=12;
   else if (val[1])
      *res=1;
   return;
}
void conTest(int *adr,int *res)
{
  // char buftmp[12];
  // if (fread(buftmp,1,4,adr)) {};
   Rprintf("adr=%d\n",*adr);
   return;
}
void readBsqLineInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if ((datatype==1)||(datatype==11))
      datasize=1;
   else if ((datatype==2)||(datatype==12))
      datasize=2;
   else if ((datatype==3)||(datatype==13))
      datasize=4;
   int i,j,adr;
   char *buf1=malloc(samples*count*datasize);
   char *buf2=malloc(8);
   int n=count*samples;
   for (i=0;i<bands;i++)
   {
      fseeko64(Fin,(long long int)(i*lines+index[0]-1)*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,n,Fin)) {};
      for (j=0;j<samples*count;j++)
      {
         adr=i*samples*count+j;
         switch(datatype)
         {
            case 1:
               res[adr]=(int)((unsigned char *)buf1)[j];
            break;
            case 11:
               res[adr]=(int)(buf1)[j];
            break;
            case 2:
               if (!swap)
               {
                  res[adr]=(int)((short *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+1];
                  buf2[1]=buf1[j*datasize+0];
                  buf2[2]=0;
                  res[adr]=(int)((short *)buf2)[0];
               }
            break;
            case 12:
               if (!swap)
               {
                  res[adr]=(int)((unsigned short *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+1];
                  buf2[1]=buf1[j*datasize+0];
                  buf2[2]=0;
                  res[adr]=(int)((unsigned short *)buf2)[0];
               }
            break;
            case 3:
               if (!swap)
               {
                  res[adr]=(int)((long *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+3];
                  buf2[1]=buf1[j*datasize+2];
                  buf2[2]=buf1[j*datasize+1];
                  buf2[3]=buf1[j*datasize+0];
                  buf2[4]=0;
                  res[adr]=(int)((long *)buf2)[0];
               }
            break;
            default:
            break;
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBsqLineDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if (datatype==4)
      datasize=4;
   else if (datatype==5)
      datasize=8;
   int i,j,adr;
   char *buf1=malloc(samples*count*datasize);
   char *buf2=malloc(9);
   int n=count*samples;
   for (i=0;i<bands;i++)
   {
      fseeko64(Fin,(long long int)(i*lines+index[0]-1)*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,n,Fin)) {};
      for (j=0;j<samples*count;j++)
      {
         adr=i*samples*count+j;
         switch(datatype)
         {
            case 4:
               if (!swap)
               {
                  res[adr]=(double)((float *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+3];
                  buf2[1]=buf1[j*datasize+2];
                  buf2[2]=buf1[j*datasize+1];
                  buf2[3]=buf1[j*datasize+0];
                  buf2[4]=0;
                  res[adr]=(double)((float *)buf2)[0];
               }
            break;
            case 5:
               if (!swap)
               {
                  res[adr]=(double)((double *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+7];
                  buf2[1]=buf1[j*datasize+6];
                  buf2[2]=buf1[j*datasize+5];
                  buf2[3]=buf1[j*datasize+4];
                  buf2[4]=buf1[j*datasize+3];
                  buf2[5]=buf1[j*datasize+2];
                  buf2[6]=buf1[j*datasize+1];
                  buf2[7]=buf1[j*datasize+0];
                  buf2[8]=0;
                  res[adr]=(double)((double *)buf2)[0];
               }
            break;
            default:
            break;
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBsqBandInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res)
{
   int lines=dim[0];
   int samples=dim[1];
  // int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if ((datatype==1)||(datatype==11))
      datasize=1;
   else if ((datatype==2)||(datatype==12))
      datasize=2;
   else if ((datatype==3)||(datatype==13))
      datasize=4;
   int i,j,adr;
   char *buf1=malloc(samples*lines*datasize);
   char *buf2=malloc(8);
   int n=lines*samples;
  // printf("file=%s datatype=%d datasize=%d n=%d count=%d nl=%d ns=%d nb=***\n"
  //       ,fname[0],datatype,datasize,n,count,lines,samples);
   for (i=0;i<count;i++)
   {
     // printf(" %d",index[i]);
      fseeko64(Fin,(long long int)(index[i]-1)*lines*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,n,Fin)) {};
      for (j=0;j<samples*lines;j++)
      {
         adr=i*samples*lines+j;
         switch(datatype)
         {
            case 1:
               res[adr]=(int)((unsigned char *)buf1)[j];
            break;
            case 11:
               res[adr]=(int)(buf1)[j];
            break;
            case 2:
               if (!swap)
               {
                  res[adr]=(int)((short *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+1];
                  buf2[1]=buf1[j*datasize+0];
                  buf2[2]=0;
                  res[adr]=(int)((short *)buf2)[0];
               }
            break;
            case 12:
               if (!swap)
               {
                  res[adr]=(int)((unsigned short *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+1];
                  buf2[1]=buf1[j*datasize+0];
                  buf2[2]=0;
                  res[adr]=(int)((unsigned short *)buf2)[0];
               }
            break;
            case 3:
               if (!swap)
               {
                  res[adr]=(int)((long *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+3];
                  buf2[1]=buf1[j*datasize+2];
                  buf2[2]=buf1[j*datasize+1];
                  buf2[3]=buf1[j*datasize+0];
                  buf2[4]=0;
                  res[adr]=(int)((long *)buf2)[0];
               }
            break;
            default:
            break;
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBsqBandDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res)
{
   int lines=dim[0];
   int samples=dim[1];
  // int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if (datatype==4)
      datasize=4;
   else if (datatype==5)
      datasize=8;
   int i,j,adr;
   char *buf1=malloc(samples*lines*datasize);
   char *buf2=malloc(9);
   int n=lines*samples;
  // printf("file=%s datatype=%d n=%d count=%d\n",fname[0],datatype,n,count);
   for (i=0;i<count;i++)
   {
     // Rprintf(" %d",index[i]);
      fseeko64(Fin,(long long int)(index[i]-1)*lines*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,n,Fin)) {};
      for (j=0;j<samples*lines;j++)
      {
         adr=i*samples*lines+j;
         switch(datatype)
         {
            case 4:
               if (!swap)
               {
                  res[adr]=(double)((float *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+3];
                  buf2[1]=buf1[j*datasize+2];
                  buf2[2]=buf1[j*datasize+1];
                  buf2[3]=buf1[j*datasize+0];
                  buf2[4]=0;
                  res[adr]=(double)((float *)buf2)[0];
               }
            break;
            case 5:
               if (!swap)
               {
                  res[adr]=(double)((double *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+7];
                  buf2[1]=buf1[j*datasize+6];
                  buf2[2]=buf1[j*datasize+5];
                  buf2[3]=buf1[j*datasize+4];
                  buf2[4]=buf1[j*datasize+3];
                  buf2[5]=buf1[j*datasize+2];
                  buf2[6]=buf1[j*datasize+1];
                  buf2[7]=buf1[j*datasize+0];
                  buf2[8]=0;
                  res[adr]=(double)((double *)buf2)[0];
                  Rprintf(" %f",res[adr]);
               }
            break;
            default:
            break;
         }
      }
   }
  // Rprintf("\n");
   free(buf1);
   fclose(Fin);
   return;
}
void readBilLineInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res)
{
  // int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if ((datatype==1)||(datatype==11))
      datasize=1;
   else if ((datatype==2)||(datatype==12))
      datasize=2;
   else if ((datatype==3)||(datatype==13))
      datasize=4;
   int i,j,adr;
   char *buf1=malloc(samples*bands*datasize);
   char *buf2=malloc(8);
   int n=bands*samples;
   for (i=0;i<count;i++)
   {
     // printf("seek=%d(%d)\n",(index[i]-1)*bands*samples*datasize,index[i]);
      fseeko64(Fin,(long long int)(index[i]-1)*bands*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,n,Fin)) {};
      for (j=0;j<samples*bands;j++)
      {
         adr=i*samples*bands+j;
         switch(datatype)
         {
            case 1:
               res[adr]=(int)((unsigned char *)buf1)[j];
            break;
            case 11:
               res[adr]=(int)(buf1)[j];
            break;
            case 2:
               if (!swap)
               {
                  res[adr]=(int)((short *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+1];
                  buf2[1]=buf1[j*datasize+0];
                  buf2[2]=0;
                  res[adr]=(int)((short *)buf2)[0];
               }
            break;
            case 12:
               if (!swap)
               {
                  res[adr]=(int)((unsigned short *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+1];
                  buf2[1]=buf1[j*datasize+0];
                  buf2[2]=0;
                  res[adr]=(int)((unsigned short *)buf2)[0];
               }
            break;
            case 3:
               if (!swap)
               {
                  res[adr]=(int)((long *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+3];
                  buf2[1]=buf1[j*datasize+2];
                  buf2[2]=buf1[j*datasize+1];
                  buf2[3]=buf1[j*datasize+0];
                  buf2[4]=0;
                  res[adr]=(int)((long *)buf2)[0];
               }
            break;
            default:
            break;
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBilLineInteger2(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if ((datatype==1)||(datatype==11))
      datasize=1;
   else if ((datatype==2)||(datatype==12))
      datasize=2;
   else if ((datatype==3)||(datatype==13))
      datasize=4;
   int b,l,s,adrIn,adrOut;
   int bs=bands*samples;
   char *buf1=malloc(bs*datasize);
   char *buf2=malloc(8);
   for (l=0;l<count;l++)
   {
     // printf("seek=%d(%d)\n",(index[i]-1)*bands*samples*datasize,index[i]);
      fseeko64(Fin,(long long int)(index[l]-1)*bands*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,bs,Fin)) {};
      for (b=0;b<bands;b++)
      {
         for (s=0;s<samples;s++)
         {
            adrOut=b*lines*samples+l*samples+s;
            adrIn=b*samples+s;
            switch(datatype)
            {
               case 1:
                  res[adrOut]=(int)((unsigned char *)buf1)[adrIn];
               break;
               case 11:
                  res[adrOut]=(int)(buf1)[adrIn];
               break;
               case 2:
                  if (!swap)
                  {
                     res[adrOut]=(int)((short *)buf1)[adrIn];
                  }
                  else
                  {
                     buf2[0]=buf1[adrIn*datasize+1];
                     buf2[1]=buf1[adrIn*datasize+0];
                     buf2[2]=0;
                     res[adrOut]=(int)((short *)buf2)[0];
                  }
               break;
               case 12:
                  if (!swap)
                  {
                     res[adrOut]=(int)((unsigned short *)buf1)[adrIn];
                  }
                  else
                  {
                     buf2[0]=buf1[adrIn*datasize+1];
                     buf2[1]=buf1[adrIn*datasize+0];
                     buf2[2]=0;
                     res[adrOut]=(int)((unsigned short *)buf2)[0];
                  }
               break;
               case 3:
                  if (!swap)
                  {
                     res[adrOut]=(int)((long *)buf1)[adrIn];
                  }
                  else
                  {
                     buf2[0]=buf1[adrIn*datasize+3];
                     buf2[1]=buf1[adrIn*datasize+2];
                     buf2[2]=buf1[adrIn*datasize+1];
                     buf2[3]=buf1[adrIn*datasize+0];
                     buf2[4]=0;
                     res[adrOut]=(int)((long *)buf2)[0];
                  }
               break;
               default:
               break;
            }
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBilLineDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res)
{
  // int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if (datatype==4)
      datasize=4;
   else if (datatype==5)
      datasize=8;
   int i,j,adr;
   char *buf1=malloc(samples*bands*datasize);
   char *buf2=malloc(9);
   int n=bands*samples;
   for (i=0;i<count;i++)
   {
      fseeko64(Fin,(long long int)(index[i]-1)*bands*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,n,Fin)) {};
      for (j=0;j<samples*bands;j++)
      {
         adr=i*samples*bands+j;
         switch(datatype)
         {
            case 4:
               if (!swap)
               {
                  res[adr]=(double)((float *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+3];
                  buf2[1]=buf1[j*datasize+2];
                  buf2[2]=buf1[j*datasize+1];
                  buf2[3]=buf1[j*datasize+0];
                  buf2[4]=0;
                  res[adr]=(double)((float *)buf2)[0];
               }
            break;
            case 5:
               if (!swap)
               {
                  res[adr]=(double)((double *)buf1)[j];
               }
               else
               {
                  buf2[0]=buf1[j*datasize+7];
                  buf2[1]=buf1[j*datasize+6];
                  buf2[2]=buf1[j*datasize+5];
                  buf2[3]=buf1[j*datasize+4];
                  buf2[4]=buf1[j*datasize+3];
                  buf2[5]=buf1[j*datasize+2];
                  buf2[6]=buf1[j*datasize+1];
                  buf2[7]=buf1[j*datasize+0];
                  buf2[8]=0;
                  res[adr]=(double)((double *)buf2)[0];
               }
            break;
            default:
            break;
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBilLineDouble2(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if (datatype==4)
      datasize=4;
   else if (datatype==5)
      datasize=8;
   int l,b,s,adrIn,adrOut;
   char *buf1=malloc(samples*bands*datasize);
   char *buf2=malloc(9);
   int bs=bands*samples;
   for (l=0;l<count;l++)
   {
      fseeko64(Fin,(long long int)(index[l]-1)*bands*samples*datasize,SEEK_SET);
      if (fread(buf1,datasize,bs,Fin)) {};
      for (b=0;b<bands;b++)
      {
         for (s=0;s<samples;s++)
         {
           // adrOut=l*bs+b*samples+s;
            adrOut=b*lines*samples+l*samples+s;
            adrIn=b*samples+s;
            switch(datatype)
            {
               case 4:
                  if (!swap)
                  {
                     res[adrOut]=(double)((float *)buf1)[adrIn];
                  }
                  else
                  {
                     buf2[0]=buf1[adrIn*datasize+3];
                     buf2[1]=buf1[adrIn*datasize+2];
                     buf2[2]=buf1[adrIn*datasize+1];
                     buf2[3]=buf1[adrIn*datasize+0];
                     buf2[4]=0;
                     res[adrOut]=(double)((float *)buf2)[0];
                  }
               break;
               case 5:
                  if (!swap)
                  {
                     res[adrOut]=(double)((double *)buf1)[adrIn];
                  }
                  else
                  {
                     buf2[0]=buf1[adrIn*datasize+7];
                     buf2[1]=buf1[adrIn*datasize+6];
                     buf2[2]=buf1[adrIn*datasize+5];
                     buf2[3]=buf1[adrIn*datasize+4];
                     buf2[4]=buf1[adrIn*datasize+3];
                     buf2[5]=buf1[adrIn*datasize+2];
                     buf2[6]=buf1[adrIn*datasize+1];
                     buf2[7]=buf1[adrIn*datasize+0];
                     buf2[8]=0;
                     res[adrOut]=(double)((double *)buf2)[0];
                  }
               break;
               default:
               break;
            }
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBilBandInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if ((datatype==1)||(datatype==11))
      datasize=1;
   else if ((datatype==2)||(datatype==12))
      datasize=2;
   else if ((datatype==3)||(datatype==13))
      datasize=4;
   int i,j,k,adr;
  // Rprintf("malloc=%ld max=%ld\n",samples*count*datasize,MAX_INT);
   char *buf1=malloc(samples*count*datasize);
   char *buf2=malloc(8);
   int ret;
   long long offset,o1,o2;
   int MAXINT=2147483647;
   for (i=0;i<lines;i++)
   {
      for (k=0;k<count;k++)
      {
        // Rprintf("fseek=%d\n",(i*bands+index[k]-1)*samples*datasize-MAX_INT);
         ret=fseek(Fin,(i*bands+index[k]-1)*samples*datasize,SEEK_SET);
         if (ret)
            ret=fseeko64(Fin,(long long int)(i*bands+index[k]-1)*samples*datasize,SEEK_SET);
         if (ret) {
            o1=(long long)(i*bands+index[k]-1)*samples*datasize;
            o2=(long long)0;
            ret=fseek(Fin,MAXINT,SEEK_SET);
            offset=o1-MAXINT;
            o2+=MAXINT;
            Rprintf("fseek0[%04d][%04d]=%d o1=%lld o2=%lld\n",i,index[k],ret,o1,o2);
            while (offset>MAXINT) {
               ret=fseek(Fin,MAXINT,SEEK_CUR);
               Rprintf("   fseekI=%d\n",ret);
               offset-=MAXINT;
            }
            fseek(Fin,offset,SEEK_CUR);
            o2+=offset;
            Rprintf("      offset: %lld %lld %lld\n",o1,o2,o1-o2);
         }
        // Rprintf("fseek=%d\n",ret);
        // fsetpos(Fin,(i*bands+index[k]-1)*samples*datasize);
         if (fread(buf1,datasize,samples,Fin)) {};
         for (j=0;j<samples;j++)
         {
            adr=i*samples*count+k*samples+j;
            switch(datatype)
            {
               case 1:
                  res[adr]=(int)((unsigned char *)buf1)[j];
               break;
               case 11:
                  res[adr]=(int)(buf1)[j];
               break;
               case 2:
                  if (!swap)
                  {
                     res[adr]=(int)((short *)buf1)[j];
                  }
                  else
                  {
                     buf2[0]=buf1[j*datasize+1];
                     buf2[1]=buf1[j*datasize+0];
                     buf2[2]=0;
                     res[adr]=(int)((short *)buf2)[0];
                  }
               break;
               case 12:
                  if (!swap)
                  {
                     res[adr]=(int)((unsigned short *)buf1)[j];
                  }
                  else
                  {
                     buf2[0]=buf1[j*datasize+1];
                     buf2[1]=buf1[j*datasize+0];
                     buf2[2]=0;
                     res[adr]=(int)((unsigned short *)buf2)[0];
                  }
               break;
               case 3:
                  if (!swap)
                  {
                     res[adr]=(int)((long *)buf1)[j];
                  }
                  else
                  {
                     buf2[0]=buf1[j*datasize+3];
                     buf2[1]=buf1[j*datasize+2];
                     buf2[2]=buf1[j*datasize+1];
                     buf2[3]=buf1[j*datasize+0];
                     buf2[4]=0;
                     res[adr]=(int)((long *)buf2)[0];
                  }
               break;
               default:
               break;
            }
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void readBilBandDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int count=*nindex;
   int datatype=*dtype;
   int swap=*byteorder;
   FILE *Fin;
   Fin=fopen(fname[0],"rb");
   if (Fin==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if (datatype==4)
      datasize=4;
   else if (datatype==5)
      datasize=8;
   int i,j,k,adr;
   char *buf1=malloc(samples*count*datasize);
   char *buf2=malloc(9);
   for (i=0;i<lines;i++)
   {
      for (k=0;k<count;k++)
      {
         fseeko64(Fin,(long long int)(i*bands+index[k]-1)*samples*datasize,SEEK_SET);
         if (fread(buf1,datasize,samples,Fin)) {};
         for (j=0;j<samples;j++)
         {
            adr=i*samples*count+k*samples+j;
            switch(datatype)
            {
               case 4:
                  if (!swap)
                  {
                     res[adr]=(double)((float *)buf1)[j];
                  }
                  else
                  {
                     buf2[0]=buf1[j*datasize+3];
                     buf2[1]=buf1[j*datasize+2];
                     buf2[2]=buf1[j*datasize+1];
                     buf2[3]=buf1[j*datasize+0];
                     buf2[4]=0;
                     res[adr]=(double)((float *)buf2)[0];
                  }
               break;
               case 5:
                  if (!swap)
                  {
                     res[adr]=(double)((double *)buf1)[j];
                  }
                  else
                  {
                     buf2[0]=buf1[j*datasize+7];
                     buf2[1]=buf1[j*datasize+6];
                     buf2[2]=buf1[j*datasize+5];
                     buf2[3]=buf1[j*datasize+4];
                     buf2[4]=buf1[j*datasize+3];
                     buf2[5]=buf1[j*datasize+2];
                     buf2[6]=buf1[j*datasize+1];
                     buf2[7]=buf1[j*datasize+0];
                     buf2[8]=0;
                     res[adr]=(double)((double *)buf2)[0];
                  }
               break;
               default:
               break;
            }
         }
      }
   }
   free(buf1);
   fclose(Fin);
   return;
}
void writeBilBandInteger(char **fname,int *value,int *dim,int *index
                        ,int *nindex,int *dtype,int *byteorder)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];//bands==counts
   int datatype=*dtype;
   int swap=*byteorder;
   int count=*nindex;
   FILE *Fout;
   Fout=fopen(fname[0],"rb+");
   if (Fout==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if ((datatype==1)||(datatype==11))
      datasize=1;
   else if ((datatype==2)||(datatype==12))
      datasize=2;
   else if ((datatype==3)||(datatype==13))
      datasize=4;
   Rprintf("datatype=%d datasize=%d lines=%d samples=%d bands=%d ind=%d:%d\n"
         ,datatype,datasize,lines,samples,bands,index[0],index[count-1]);
   int i,j,k,adr,adr2;
   int n=samples*count;
   char *buf1=malloc(samples*count*datasize);
   char buf2[8];
   for (i=0;i<lines;i++)
   {
     // printf("line=%d:",(i*bands+(index[0]-1))*samples*datasize);
      fseek(Fout,(i*bands+index[0]-1)*samples*datasize,SEEK_SET);
      for (j=0;j<count;j++)
      {
         adr=j*samples*lines+i*samples;
        // printf("$%d",adr);
         for (k=0;k<samples;k++)
         {
            adr2=(j*samples+k)*datasize;
            if (!swap)
               memcpy(buf1+adr2,value+adr+k,datasize);
            else
            {
               memcpy(buf2,value+adr+k,datasize);
               switch(datatype)
               {
                  case 2:case 12:
                     buf1[adr2+0]=buf2[1];
                     buf1[adr2+1]=buf2[0];
                  break;
                  case 3:case 13:
                     buf1[adr2+0]=buf2[3];
                     buf1[adr2+1]=buf2[2];
                     buf1[adr2+2]=buf2[1];
                     buf1[adr2+3]=buf2[0];
                  break;
                  default:
                  break;
               }
            }
         }
      }
      fwrite(buf1,datasize,n,Fout);
     // printf("\n");
   }
   free(buf1);
   fclose(Fout);
   return;
}
void writeBilBandDouble(char **fname,double *value,int *dim,int *index
                        ,int *nindex,int *dtype,int *byteorder)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];//bands==counts
   int datatype=*dtype;
   int swap=*byteorder;
   int count=*nindex;
   FILE *Fout;
   Fout=fopen(fname[0],"rb+");
   if (Fout==NULL)
      REprintf("%s not found\n",fname[0]);
   int datasize=0;
   if (datatype==4)
      datasize=4;
   else if (datatype==5)
      datasize=8;
   int i,j,k,adr;
   int n=samples;
   char *buf1=malloc(samples*datasize);
   char buf2[9];
   for (i=0;i<lines;i++)
   {
      fseek(Fout,(i*bands+index[0]-1)*samples*datasize,SEEK_SET);
      for (j=0;j<count;j++)
      {
         adr=j*samples*lines+i*samples;
         for (k=0;k<samples;k++)
         {
            if (!swap)
               memcpy(buf1+k*datasize,value+adr+k,datasize);
            else
            {
               memcpy(buf2,value+adr+k,datasize);
               switch(datatype)
               {
                  case 4:
                     buf1[k*datasize+0]=buf2[3];
                     buf1[k*datasize+1]=buf2[2];
                     buf1[k*datasize+2]=buf2[1];
                     buf1[k*datasize+3]=buf2[0];
                  break;
                  case 5:
                     buf1[k*datasize+0]=buf2[7];
                     buf1[k*datasize+1]=buf2[6];
                     buf1[k*datasize+2]=buf2[5];
                     buf1[k*datasize+3]=buf2[4];
                     buf1[k*datasize+4]=buf2[3];
                     buf1[k*datasize+5]=buf2[2];
                     buf1[k*datasize+6]=buf2[1];
                     buf1[k*datasize+7]=buf2[0];
                  break;
                  default:
                  break;
               }
            }
         }
         fwrite(buf1,datasize,n,Fout);
      }
   }
   free(buf1);
   fclose(Fout);
   return;
}
void timefilt4(double *x,int *dim,int *win,double *cover,double *res)
{
   int samples=dim[0];
   int bands=dim[1];
   int sizet=*win;
   int i,c,t,adr,adr2;
   int shiftt=(int)(sizet/2);
   double Mx;
   int n,n2;
  // printf("dim=c(%d,%d)\n",samples,bands);
   for (c=0;c<samples;c++)
   {
      adr=c;
      for (t=0;t<bands;t++,adr+=samples)
      {
        // if (ISNA(x[adr]))
        //    continue;
         Mx=0.0;
         n=n2=0;
         for (i=-shiftt;i<=shiftt;i++)
         {
            if (t+i<0 || t+i>=bands)
               continue;
            n2++;
            adr2=adr+i*samples;
            if (ISNA(x[adr2]))
               continue;
            Mx+=x[adr2];
            n++;
         }
         res[adr]=NA_REAL;
        // printf(" %d(%d)",n,n2);
         if ((float)n/(float)n2<*cover)
            res[adr]=NA_REAL;
         else
            res[adr]=Mx/(float)n;
      }
   }
  // printf("\n");
   return;
}
void interp4(double *x,int *dim,int *win,double *cover,double *res)
{
   int samples=dim[0];
   int bands=dim[1];
   int sizet=*win;
   int i,c,t,adr,adr2;
   int shiftt=(int)(sizet/2);
   double left,right;
   int iL,iR;
   if (0)
   {
      Rprintf("shiftt=%d samples=%d bands=%d\n",shiftt,samples,bands);
      if (0)
      {
         i=1;
         for (c=0;c<samples;c++)
         {
            for (t=0;t<bands;t++,adr+=samples,i++)
            {
               adr=c*bands+t;
               Rprintf(" %d:%f",ISNA(x[adr]),x[adr]);
               if (i>128)
                  break;
            }
            if (i>128)
               break;
         }
        // exit(EXIT_FAILURE);
      }
      if (0)
      {
         for (i=-1;i>=-shiftt;i--)
            Rprintf(" %d",i);
         Rprintf("\n");
         for (i=1;i<=shiftt;i++)
            Rprintf(" %d",i);
      }
      Rprintf("\n");
   }
   for (c=0;c<samples;c++)
   {
      adr=c;
      for (t=0;t<bands;t++,adr+=samples)
      {
         res[adr]=x[adr];
         if (!ISNA(x[adr]))
            continue;
        // printf(" %d",t);
         left=right=NA_REAL;
         iL=iR=0;
         for (i=-1;i>=-shiftt;i--)
         {
            if (t+i<0 || t+i>=bands)
               continue;
            adr2=adr+i*samples;
            if (ISNA(x[adr2]))
               continue;
            left=x[adr2];
            iR=-i;
            break;
         }
         for (i=1;i<=shiftt;i++)
         {
            if (t+i<0 || t+i>=bands)
               continue;
            adr2=adr+i*samples;
            if (ISNA(x[adr2]))
               continue;
            right=x[adr2];
            iL=i;
            break;
         }
        // printf("adr=%d left=%f right=%f\n",adr,left,right);exit(EXIT_FAILURE);
         if ((!ISNA(left))&&(!ISNA(right)))
            res[adr]=(iL*left+iR*right)/(iL+iR);
         else if (!ISNA(left))
            res[adr]=left;
         else if (!ISNA(right))
            res[adr]=right;
      }
   }
  // printf("\n");
   return;
}
void aggregate(double *x,int *dim,int *S,double *cvr,int *verb,double *res)
{
   //~ return;
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   float cover=(float)*cvr;
   int size=*S;
   int verbose=*verb;
   int samples2=(int)ceil(samples/size);
   int lines2=(int)ceil(lines/size);
   int c,r,t,adr,adr1,adr2,adr3,c2,r2;
   int maxadr3=samples*lines*bands;
   double Mout;
   float nout;
   if (verbose)
      Rprintf("aggregate: r=%d c=%d r2=%d c2=%d b=%d size=%d maxAdr3=%d\n"
            ,lines,samples,lines2,samples2,bands,size,maxadr3);
   for (adr=0;adr<samples2*lines2*bands;adr++)
      res[adr]=NA_REAL;
   for (t=0;t<bands;t++)
   {
      adr1=t*lines*samples;
      for (r=0;r<lines2;r++)
      {
         for (c=0;c<samples2;c++)
         {
            adr2=t*lines2*samples2+r*samples2+c;
            Mout=0.0;
            nout=0;
            for (r2=0;r2<size;r2++)
            {
               for (c2=0;c2<size;c2++)
               {
                  adr3=adr1+r*size*samples+r2*samples+c*size+c2;
                  if (adr3>=maxadr3)
                  {
                     Rprintf(" %d",adr3);
                    // exit(EXIT_FAILURE);
                  }
                  if (ISNA(x[adr3]))
                     continue;
                  Mout+=x[adr3];
                  nout++;
               }
            }
            if ((nout>0.0)&&(nout>=size*size*cover))
               res[adr2]=Mout/nout;
         }
      }
   }
   return;
}
void expand(double *x,int *dim,int *S,int *verb,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   int size=*S;
   int verbose=*verb;
   int samples2=samples*size;
   int lines2=lines*size;
   int c,r,t,adr,adr1,adr2,c2,r2;
   if (verbose)
      Rprintf("expand: r=%d c=%d r2=%d c2=%d b=%d size=%d\n"
            ,lines,samples,lines2,samples2,bands,size);
   for (adr=0;adr<samples2*lines2*bands;adr++)
      res[adr]=NA_REAL;
   for (t=0;t<bands;t++)
   {
      for (r=0;r<lines;r++)
      {
         for (c=0;c<samples;c++)
         {
            adr1=t*lines*samples+r*samples+c;
            //~ if (ISNA(x[adr1]))
               //~ continue;
            for (r2=0;r2<size;r2++)
            {
               for (c2=0;c2<size;c2++)
               {
                  adr2=t*lines2*samples2+(r*size+r2)*samples2+(c*size+c2);
                  res[adr2]=x[adr1];
               }
            }
         }
      }
   }
   return;
}
void bilinear(double *src,int *dim,int *S,int *verb,double *dst)
{
   int samples1=dim[0];
   int lines1=dim[1];
   int bands=dim[2];
   int size=*S;
   int verbose=*verb;
   int samples2=samples1*size;
   int lines2=lines1*size;
   int t,adr,adr1,adrLD,adrLU,adrRD,adrRU,c2,r2;
   if (verbose)
      Rprintf("bilinear: r=%d c=%d r2=%d c2=%d b=%d size=%d\n"
            ,lines1,samples1,lines2,samples2,bands,size);
   for (adr=0;adr<samples2*lines2*bands;adr++)
      dst[adr]=NA_REAL;
   double x2,y2;
   int cL,cR,rD,rU;
   double xL,xR,yD,yU;
  // double xL0,xR0,yD0,yU0;
   double M,w,wLD,wLU,wRD,wRU;
   int mul=1;
   for (t=0;t<bands;t++)
   {
      adr1=t*lines1*samples1;
      for (r2=0;r2<lines2;r2++)
      {
         y2=((float)r2+0.5)/(float)size;
         rD=(int)(floor(y2-0.5)+0.0);
         rU=(int)(ceil(y2-0.5)+0.0);
        // yD=ceil(r2/(float)size)-y2+0.5; // src
         yD=(int)rU+0.5-y2;
         if (rD<0-999*mul)
         {
            Rprintf("HERE\n");
            yD=yD+1.0;
            rD++;
            rU++;
         }
         if (rU>=lines1+999*mul)
         {
            Rprintf("HERE\n");
            yD=yD-1.0;
            rD--;
            rU--;
         }
         yU=1.0-yD;
         if ((1)&&(verbose)&&(t==0)&&((r2<-12)||(r2>=(lines2+12))))
            Rprintf("r2=%d y2=%.2f rD=%d rU=%d yD=%.2f yU=%.2f\n"
                   ,r2,y2,rD,rU,yD,yU);
         for (c2=0;c2<samples2;c2++)
         {
            x2=((float)c2+0.5)/(float)size;
            cL=(int)(floor(x2-0.5)+0.0);
            cR=(int)(ceil(x2-0.5)+0.0);
           // xL=ceil(c2/(float)size)-x2+0.5; src
            xL=(float)cR+0.5-x2;
            if (cL<0-999*mul)
            {
               Rprintf("HERE\n");
               xL=xL+1.0;
               cL++;
               cR++;
            }
            if (cR>=samples1+999*mul)
            {
               Rprintf("HERE\n");
               xL=xL-1.0;
               cL--;
               cR--;
            }
            xR=1.0-xL;
            adr=t*lines2*samples2+r2*samples2+c2;
            adrLD=adr1+rD*samples1+cL;
            adrLU=adr1+rU*samples1+cL;
            adrRD=adr1+rD*samples1+cR;
            adrRU=adr1+rU*samples1+cR;
            M=w=0.0;
           /*
             ABC
             DEF
             GHI
           */
            if ((cL>=0)&&(cR<samples1)&&(rD>=0)&&(rU<lines1)) // E
            {
               wLD=xL*yD;
               wLU=xL*yU;
               wRD=xR*yD;
               wRU=xR*yU;
            }
            else if (rD<0) // ABC
            {
               wLD=0.0;
               wRD=0.0;
               if (cL<0) // A
               {
                  wLU=0.0;
                  wRU=1.0;
               }
               else if (cR>=samples1) // C
               {
                  wLU=1.0;
                  wRU=0.0;
               }
               else // B
               {
                  wLU=xL;
                  wRU=xR;
               }
            }
            else if (rU>=lines1) // GHI
            {
               wLU=0.0;
               wRU=0.0;
               if (cL<0) // G
               {
                  wLD=0.0;
                  wRD=1.0;
               }
               else if (cR>=samples1) // H
               {
                  wLD=1.0;
                  wRD=0.0;
               }
               else // I
               {
                  wLD=xL;
                  wRD=xR;
               }
            }
            else if (cL<0) // D
            {
               wLD=0.0;
               wLU=0.0;
               wRD=yD;
               wRU=yU;
            }
            else if (cR>=samples1) // F
            {
               wLD=yD;
               wLU=yU;
               wRD=0.0;
               wRU=0.0;
            }
            else //
            {
               Rprintf("unexpected!\n");
               return;
               wLD=0.0;
               wLU=0.0;
               wRD=0.0;
               wRU=0.0;
            }
            if ((wLD>0)&&(!ISNA(src[adrLD])))
            {
               M+=(src[adrLD]*wLD);
               w+=wLD;
            }
            if ((wLU>0)&&(!ISNA(src[adrLU])))
            {
               M+=(src[adrLU]*wLU);
               w+=wLU;
            }
            if ((wRD>0)&&(!ISNA(src[adrRD])))
            {
               M+=(src[adrRD]*wRD);
               w+=wRD;
            }
            if ((wRU>0)&&(!ISNA(src[adrRU])))
            {
               M+=(src[adrRU]*wRU);
               w+=wRU;
            }
            if (w==0.0)
               continue;
            if (w<1.0)
               dst[adr]=M/w;
            //~ else if (w>1.0)
               //~ dst[adr]=M/w;
            else
               dst[adr]=M;
            if ((1)&&(verbose)&&(t==-1)&&(c2==10-1)&&(r2==1-1))
            {
               Rprintf(" x2=%.2f y2=%.2f c2=%d r2=%d\n"
                       " cL=%d cR=%d rD=%d rU=%d"
                       " xL=%.2f xR=%.2f yD=%.2f yU=%.2f\n"
                      // " adrLD=%d adrLU=%d adrRD=%d adrRU=%d\n"
                      // " srcLD=%.4f srcLU=%.4f srcRD=%.4f srcRU=%.4f\n"
                       " wLD=%.4f wLU=%.4f wRD=%.4f wRU=%.4f w=%.4f\n"
                       " res=%.4f\n"
                      ,x2,y2,c2,r2,cL,cR,rD,rU,xL,xR,yD,yU
                      //,adrLD,adrLU,adrRD,adrRU
                      //,src[adrLD],src[adrLU],src[adrRD],src[adrRU]
                      ,wLD,wLU,wRD,wRU,w,dst[adr]);
            }
            if ((0)&&(verbose)&&(dst[adr]>100))
            {
               Rprintf("t=%d c2=%d r2=%d w=%.4f M=%.1f %.1f\n"
                      ,t,c2,r2,w,M,dst[adr]);
               return;
            }
         }
      }
   }
   return;
}
void table_log(int *x,int *size,int *res)
{
   int i;
   int r=0;
   for (i=0;i<*size;i++)
   {
      if (!x[i])
         continue;
      r=1;
      break;
   }
   *res=r;
  // Rprintf("r=%d i=%d size=%d\n",r,i,*size);
   return;
}
void rasterize(double *img,int *dim,double *bbox
              ,double *crdx,double *crdy,double *value,double *nodata
              ,int *len,int *_kind)
{
   double minx=bbox[0];
   double miny=bbox[1];
   double maxx=bbox[2];
   double maxy=bbox[3];
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   int n=*len;
   int kind=*_kind;
   int adr1,adr2,t,i,c,r;
   int debug=0;
   double bg=*nodata;
   int *valid=(int *)malloc(n*sizeof(int));
   double *S=(double *)malloc(samples*lines*sizeof(double));
   int *N=(int *)malloc(samples*lines*sizeof(int));
   if (debug)
      Rprintf("minx=%f miny=%f maxx=%f maxy=%f columns=%d rows=%d\n"
             ,minx,miny,maxx,maxy,samples,lines);
   for (i=0;i<n;i++)
   {
      valid[i]=-1;
      c=(int)floor(samples*(crdx[i]-minx)/(maxx-minx));
      if ((c<0)||(c>=samples))
         continue;
      r=(int)(lines-1-(int)floor(lines*(crdy[i]-miny)/(maxy-miny)));
      if ((r<0)||(r>=lines))
         continue;
      valid[i]=r*samples+c;
      if (debug>1)
         Rprintf("x=%f y=%f c=%d r=%d valid=%d value=%f\n"
                ,crdx[i],crdy[i],c,r,valid[i],value[i]);
   }
   for (t=0;t<bands;t++) // get band
   {
      adr1=t*lines*samples;
      memset(N,0,samples*lines*sizeof(int));
      memset(S,0,samples*lines*sizeof(double));
      for (i=0;i<n;i++)
      {
         adr2=valid[i];
         if (adr2<0)
            continue;
         S[adr2]+=value[i+t*n];
         N[adr2]++;
      }
      for (i=0;i<samples*lines;i++)
      {
         if (!N[i])
         {
            img[i+adr1]=bg;
            continue;
         }
         if (kind==1) // mean
            img[i+adr1]=S[i]/(double)N[i];
         else if (kind==2) // sum
            img[i+adr1]=S[i];
         else if (kind==4) // n
            img[i+adr1]=N[i];
      }
   }
   free(valid);
   free(S);
   free(N);
   valid=NULL;
   S=NULL;
   N=NULL;
   return;
}
int focalCommon(double *x,int *dim,double *bg,double *H,int *sz
                ,double *cvr,int *fz,int *E,int *verb,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   double background=*bg;
   double cover=*cvr;
   if (cover>1.0)
      cover=1.0;
   int fillNA=*fz;
   int saveMargin=*E;
   int size=*sz;
   int verbose=*verb;
   int t,r,c,r2,c2,i,j,i2,j2,k,m,adr,adr1,adr2;
   double M,sumH,maxH=1.0;
   short shift=(short)(size/2);
   double eps=1e-38;
   double small_eps=1e-1;
   short edge=0;
   int pb,pbmax=bands*lines;
   float sizex,sizey;
   sizex=sizey=(float)size;
   for (m=0,sumH=0.0;m<size*size;m++)
      sumH+=H[m];
   if (fabs(sumH)<small_eps)
      edge=1;
   if ((0)&&(sumH<-eps))
      for (m=0;m<size*size;m++)
         H[m]=-H[m];
   if (verbose)
   {
      Rprintf("focalCommon: r=%d c=%d b=%d size=%d shift=%d"
             " fillZ=%d margin=%d cover=%.2f edge=%d(%.2g) bg=%.1f\n"
            ,lines,samples,bands,size,shift,fillNA,saveMargin
            ,cover,edge,sumH,background);
      if (verbose>1)
      {
         for (j=0;j<size;j++)
         {
            for (i=0;i<size;i++)
               Rprintf(" %6.3f",H[j*size+i]);
            Rprintf("\n");
         }
      }
   }
   for (t=0,pb=0;t<bands;t++) // get band
   {
      adr=t*lines*samples;
      for (r=0;r<lines;r++,pb++)
      {
         for (c=0;c<samples;c++)
         {
            adr1=adr+r*samples+c;
            M=sumH=0.0;
            m=0;
            for (j=-shift;j<=shift;j++)
            {
               r2=r+j;
               if (r2<0 || r2>=lines)
                  continue;
               j2=j+shift;
               for (i=-shift;i<=shift;i++)
               {
                  c2=c+i;
                  if (c2<0 || c2>=samples)
                     continue;
                  i2=i+shift;
                  k=j2*size+i2;
                  adr2=adr+r2*samples+c2;
                  if (fabs(x[adr2]-background)<eps)
                     continue;
                  sumH+=H[k];
                  m++;
                  M+=(H[k]*x[adr2]);
               }
            }
            if (fillNA)
               res[adr1]=x[adr1];
            else
               res[adr1]=background;
            if (edge)
            {
               if (fabs(sumH)<eps)
                  M=M-sumH*H[shift*size+shift]; // by weight of central element
               sumH=1;
            }
            if ((1)&&(fabs(sumH)<small_eps))
               continue;
            if (saveMargin) {
               r2=r; // r-shift;
               c2=c;
               if (r2<shift)
                  sizey=shift;
               else if (lines-r2-1<shift)
                  sizey=lines-r2-1;
               else
                  sizey=size;
               if (c2<shift)
                  sizex=shift;
               else if (samples-c2-1<shift)
                  sizex=samples-c2-1;
               else
                  sizex=size;
            }
            if (((0)&&(sumH<maxH*cover))||((1)&&(m<sizex*sizey*cover)))
               continue;
           // Rprintf("%d",fabs(x[adr1]-background)<eps);
            if (((fillNA)&&(fabs(x[adr1]-background)<eps))||(!fillNA))
               res[adr1]=M/sumH;
         }
         if (verbose)
            progressBar(pb,pbmax,"");
      }
   }
   return 0;
}
void focalGaussian(double *x,int *dim,double *bg,double *sz,double *S,double *A
             ,double *cvr,int *fz,int *E,int *verb,double *res)
{
   double sigma=*S;
   int size=(short)ceil(*sz);
   if (!(size%2))
      size++;
   int shift=(short)(size/2);
   int i,j,i2,j2,k,m;
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   for (j=0;j<size;j++)
   {
      j2=j-shift;
      for (i=0;i<size;i++)
      {
         i2=i-shift;
         k=i2*i2+j2*j2;
         m=j*size+i;
         if (sqrt(k)>(shift+0.5-eps))
            H[m]=0.0;
         else
            H[m]=exp(-k/(2*sigma*sigma));
      }
   }
   focalCommon(x,dim,bg,H,&size,cvr,fz,E,verb,res);
   free(H);
   return;
}
void focalLaplacian(double *x,int *dim,double *bg,double *sz,double *S,double *A
               ,double *cvr,int *fz,int *E,int *verb,double *res)
{
   double alpha=*A;
   int size=(short)ceil(*sz);
   if (size!=3)
      size=3;
   if (!(size%2))
      size++;
   int shift=(short)(size/2);
   int i,j,i2,j2,k,m;
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   for (j=0;j<size;j++)
   {
      j2=j-shift;
      for (i=0;i<size;i++)
      {
         i2=i-shift;
         k=i2*i2+j2*j2;
         m=j*size+i;
         if (sqrt(k)>(shift+0.5-eps))
            H[m]=0.0;
         else
         {
            if ((!i2)&&(!j2))
               H[m]=-1.0;
            else if (abs(i2)==abs(j2))
               H[m]=alpha/4.0;
            else
               H[m]=(1.0-alpha)/4.0;
         }
      }
   }
   focalCommon(x,dim,bg,H,&size,cvr,fz,E,verb,res);
   free(H);
   return;
}
void focalOsisaf(double *x,int *dim,double *bg,double *sz,double *S,double *A
               ,double *cvr,int *fz,int *E,int *verb,double *res)
{
   int size=(short)ceil(*sz);
   if (size!=5)
      size=5;
   if (!(size%2))
      size++;
   int shift=(short)(size/2);
   int i,j,i2,j2,k,m;
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   for (j=0;j<size;j++)
   {
      j2=j-shift;
      for (i=0;i<size;i++)
      {
         i2=i-shift;
         k=i2*i2+j2*j2;
         m=j*size+i;
         if (sqrt(k)>99*(shift+0.5-eps))
            H[m]=-99.0;
         else
         {
            if ((!i2)&&(!j2))
               H[m]=0.0;
            else if ((abs(i2)==2)||(abs(j2)==2))
               H[m]=-1.0/16.0;
            else if ((abs(i2)==1)||(abs(j2)==1))
               H[m]=2.0/16.0;
         }
      }
   }
   focalCommon(x,dim,bg,H,&size,cvr,fz,E,verb,res);
   free(H);
   return;
}
void focalHires(double *x,int *dim,double *bg,double *sz,double *S,double *A
               ,double *cvr,int *fz,int *E,int *verb,double *res)
{
   double alpha=*A;
   int size=(short)ceil(*sz);
   if (size!=3)
      size=3;
   if (!(size%2))
      size++;
   int shift=(short)(size/2);
   int i,j,i2,j2,k,m;
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   for (j=0;j<size;j++)
   {
      j2=j-shift;
      for (i=0;i<size;i++)
      {
         i2=i-shift;
         k=i2*i2+j2*j2;
         m=j*size+i;
         if (sqrt(k)>(shift+0.5-eps))
            H[m]=0.0;
         else
         {
            if ((!i2)&&(!j2))
               H[m]=5.0;// 20170108 removed 'alpha+5.0';
            else if ((abs(i2)==1)&&(abs(j2)==1))
               H[m]=-alpha;
            else
               H[m]=alpha-1.0;
         }
      }
   }
   focalCommon(x,dim,bg,H,&size,cvr,fz,E,verb,res);
   free(H);
   return;
}
void focalCorrel(double *x,int *dim,double *bg,double *sz,double *S,double *A
            ,double *cvr,int *fz,int *E,int *verb,double *res)
{
   double alpha=*A;
   int size=(short)ceil(*sz);
   if (size!=3)
      size=3;
   if (!(size%2))
      size++;
   int shift=(short)(size/2);
   int i,j,i2,j2,k,m;
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   for (j=0;j<size;j++)
   {
      j2=j-shift;
      for (i=0;i<size;i++)
      {
         i2=i-shift;
         k=i2*i2+j2*j2;
         m=j*size+i;
         if (sqrt(k)>(shift+0.5-eps))
            H[m]=0.0;
         else
         {
            if ((!i2)&&(!j2))
               H[m]=(1+alpha*alpha)*(1+alpha*alpha);
            else if ((abs(i2)==1)&&(abs(j2)==1))
               H[m]=alpha*alpha;
            else
               H[m]=-alpha*(1.0+alpha*alpha);
         }
      }
   }
   focalCommon(x,dim,bg,H,&size,cvr,fz,E,verb,res);
   free(H);
   return;
}
int focalSobel(double *x,int *dim,double *bg
          ,double *_size,double *_sigma,double *_alpha
          ,double *cvr,int *_fillNA,int *_saveMargin,int *verb,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   double background=*bg;
   double cover=*cvr;
   int saveMargin=*_saveMargin;
   if (cover>1.0)
      cover=1.0;
   int size=3;
   int verbose=*verb;
   int t,r,c,r2,c2,i,j,i2,j2,k,m,adr,adr1,adr2;
   short shift=(short)(size/2);
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   double X,Y;
   float sizex,sizey;
   sizex=sizey=(float)size;
   if (verbose)
   {
      Rprintf("focalSobel: r=%d c=%d b=%d size=%d shift=%d"
             " cover=%.2f margin=%d bg=%.1f\n"
            ,lines,samples,bands,size,shift,cover,saveMargin,background);
   }
   for (t=0;t<bands;t++) // get band
   {
      adr=t*lines*samples;
      for (r=0;r<lines;r++)
      {
         for (c=0;c<samples;c++)
         {
            adr1=adr+r*samples+c;
            m=0;
            for (k=0;k<size*size;k++)
               H[k]=0.0;
            for (j=-shift;j<=shift;j++)
            {
               r2=r+j;
               if (r2<0 || r2>=lines)
                  continue;
               j2=j+shift;
               for (i=-shift;i<=shift;i++)
               {
                  c2=c+i;
                  if (c2<0 || c2>=samples)
                     continue;
                  i2=i+shift;
                  k=j2*size+i2;
                  adr2=adr+r2*samples+c2;
                  if (fabs(x[adr2]-background)<eps)
                     continue;
                  H[k]=x[adr2];
                  m++;
               }
            }
            res[adr1]=background;
            if (saveMargin) {
               r2=r;// 'r-shift' if buffered input
               c2=c;
               if (r2<shift)
                  sizey=shift;
               else if (lines-r2-1<shift)
                  sizey=lines-r2-1;
               else
                  sizey=size;
               if (c2<shift)
                  sizex=shift;
               else if (samples-c2-1<shift)
                  sizex=samples-c2-1;
               else
                  sizex=size;
            }
            if ((1)&&(m<sizex*sizey*cover))
               continue;
            X=-H[2]+2*H[5]-H[8]+H[0]-2*H[3]+H[6];
            Y=-H[0]+2*H[1]-H[2]+H[6]-2*H[7]+H[8];
            res[adr1]=sqrt(X*X+Y*Y);
         }
      }
   }
   free(H);
   return 0;
}
int focalSobelG(double *x,int *dim,double *bg
          ,double *_size,double *_sigma,double *_alpha
          ,double *cvr,int *_fillNA,int *_saveMargin,int *verb,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   double background=*bg;
   double cover=*cvr;
   int saveMargin=*_saveMargin;
   if (cover>1.0)
      cover=1.0;
   int size=3;
   int verbose=*verb;
   int t,r,c,r2,c2,i,j,i2,j2,k,m,adr,adr1,adr2;
   short shift=(short)(size/2);
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   double X,Y;
   float sizex,sizey;
   sizex=sizey=(float)size;
   if (verbose)
   {
      Rprintf("focalSobelGradient: r=%d c=%d b=%d size=%d shift=%d"
             " cover=%.2f margin=%d bg=%.1f\n"
            ,lines,samples,bands,size,shift,cover,saveMargin,background);
   }
   for (t=0;t<bands;t++) // get band
   {
      adr=t*lines*samples;
      for (r=0;r<lines;r++)
      {
         for (c=0;c<samples;c++)
         {
            adr1=adr+r*samples+c;
            m=0;
            for (k=0;k<size*size;k++)
               H[k]=0.0;
            for (j=-shift;j<=shift;j++)
            {
               r2=r+j;
               if (r2<0 || r2>=lines)
                  continue;
               j2=j+shift;
               for (i=-shift;i<=shift;i++)
               {
                  c2=c+i;
                  if (c2<0 || c2>=samples)
                     continue;
                  i2=i+shift;
                  k=j2*size+i2;
                  adr2=adr+r2*samples+c2;
                  if (fabs(x[adr2]-background)<eps)
                     continue;
                  H[k]=x[adr2];
                  m++;
               }
            }
            res[adr1]=background;
            if (saveMargin) {
               r2=r;// 'r-shift' if buffered input
               c2=c;
               if (r2<shift)
                  sizey=shift;
               else if (lines-r2-1<shift)
                  sizey=lines-r2-1;
               else
                  sizey=size;
               if (c2<shift)
                  sizex=shift;
               else if (samples-c2-1<shift)
                  sizex=samples-c2-1;
               else
                  sizex=size;
            }
            if ((1)&&(m<sizex*sizey*cover))
               continue;
            X=-H[2]+2*H[5]-H[8]+H[0]-2*H[3]+H[6];
            Y=-H[0]+2*H[1]-H[2]+H[6]-2*H[7]+H[8];
            if (X!=0)
               res[adr1]=atan(Y/X);
            else if (Y>=0)
               res[adr1]=PI/2.0;
            else
               res[adr1]=-PI/2.0;
           // res[adr1]=sqrt(X*X+Y*Y);
         }
      }
   }
   free(H);
   return 0;
}
void focalLoG(double *x,int *dim,double *bg,double *sz,double *S,double *A
             ,double *cvr,int *fz,int *E,int *verb,double *res)
{
   double sigma=*S;
   double sigma2=sigma*sigma;
  // double PIsigma4=sigma2*sigma2*PI;
   double twoPIsigma6=sigma2*sigma2*sigma2*2*PI;
   double twoSigma2=2*sigma2;
   int size=(short)ceil(*sz);
   if (!(size%2))
      size++;
   int shift=(short)(size/2);
   int i,j,i2,j2,k,m;
   double eps=1e-38;
   double *H=(double *)malloc(size*size*sizeof(double));
   double localH;
   for (j=0;j<size;j++)
   {
      j2=j-shift;
      for (i=0;i<size;i++)
      {
         i2=i-shift;
         k=i2*i2+j2*j2;
         m=j*size+i;
         if (sqrt(k)>(shift+0.5-eps))
            H[m]=0.0;
         else
            H[m]=exp(-k/twoSigma2)*(k-twoSigma2)/twoPIsigma6;
           // H[m]=-(1.0/PIsigma4)*(1.0-k/twoSigma2)*exp(-k/twoSigma2);
      }
   }
   localH=H[shift*size+shift];
   for (m=0;m<size*size;m++)
      H[m]=H[m]/localH;
   focalCommon(x,dim,bg,H,&size,cvr,fz,E,verb,res);
   free(H);
   return;
}
void variability4(double *y,double *x,int *dim,double *cover,double *res)
{
   int pixels=dim[0]; // pixels=columns*lines
   int bands=dim[1];
   int p,t,adr;
   int adrMean,adrSd,adrSlope,adrF,adrN,adrSum,adrMin,adrMax,adrSSR,adrSSE;
   double Mx,Sx,Sy,My,Mxy,SSE,SSR,slope,intercept,Sum,Min,Max;
   double nf,bf=(float)bands;
   int n;
   double yhat;
   for (p=0;p<pixels;p++)
   {
      Mx=My=Mxy=Sx=Sy=SSE=SSR=Sum=0.0;
      Min=1e38;
      Max=-1e38;
      for (t=0,n=0,adr=p;t<bands;t++,adr+=pixels)
      {
         if (ISNA(y[adr]))
            continue;
         Mx+=x[t];
         Sum+=y[adr];
         Mxy+=x[t]*y[adr];
         n++;
         if (y[adr]<Min)
            Min=y[adr];
         if (y[adr]>Max)
            Max=y[adr];
      }
      nf=(double)n;
      if (n)
      {
         Mx/=nf;
         My=Sum/nf;
         Mxy/=nf;
      }
      for (t=0,adr=p;t<bands;t++,adr+=pixels)
      {
         if (ISNA(y[adr]))
            continue;
         Sx+=(x[t]-Mx)*(x[t]-My);
         Sy+=(y[adr]-My)*(y[adr]-My);
      }
      if (n)
      {
         Sx=sqrt(Sx/nf);
         Sy=sqrt(Sy/nf);
      }
      if (Sx>0)
      {
         slope=(Mxy-Mx*My)/(Sx*Sx);
         intercept=My-slope*Mx;
      }
      else
      {
         slope=0.0;
         intercept=My;
      }
      if (p==-19587-1)
         Rprintf("Mx=%f My=%f Mxy=%f Sx=%f slope=%f\n",Mx,My,Mxy,Sx,slope);
      for (t=0,adr=p;t<bands;t++,adr+=pixels)
      {
         if (ISNA(y[adr]))
            continue;
         yhat=slope*x[t]+intercept;
         SSR+=(yhat-y[adr])*(yhat-y[adr]);
         SSE+=(yhat-My    )*(yhat-My    );
      }
      adrMean=p+0*pixels;
      adrSd=p+1*pixels;
      adrSum=p+2*pixels;
      adrMin=p+3*pixels;
      adrMax=p+4*pixels;
      adrN=p+5*pixels;
      adrSlope=p+6*pixels;
      adrF=p+7*pixels;
      adrSSR=p+8*pixels;
      adrSSE=p+9*pixels;
      if ((!n)||(nf/bf<*cover))
      {
         res[adrMean]=NA_REAL;
         res[adrSd]=NA_REAL;
         res[adrSum]=NA_REAL;
         res[adrMin]=NA_REAL;
         res[adrMax]=NA_REAL;
         res[adrN]=NA_REAL;
         res[adrSlope]=NA_REAL;
         res[adrF]=NA_REAL;
         res[adrSSE]=NA_REAL;
         res[adrSSR]=NA_REAL;
      }
      else
      {
         res[adrMean]=My;
         res[adrSd]=Sy;
         res[adrSum]=Sum;
         res[adrMin]=Min;
         res[adrMax]=Max;
         res[adrN]=nf;
         if (Sx>0)
            res[adrSlope]=slope;
         else
            res[adrSlope]=0.0;
         res[adrSSR]=SSR;
         res[adrSSE]=SSE;
         if ((n>2)&&(SSR>0.0))
         {
            res[adrF]=SSE*(n-2)/SSR;
            if (slope<0)
               res[adrF]=-res[adrF];
         }
         else
            res[adrF]=0.0;
         //~ res[adrF]=1.0;
      }
   }
   return;
}
void focalMedian(double *x,double *bg,int *dim,int *S,int *F,int *E,double *cvr
            ,int *verbose,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   int size=*S;
   int toFill=*F;
   int saveMargin=*E;
   int checkadr=samples*lines*bands;
   float cover=(float)*cvr;
   short i,j;
   if (!(size%2))
      size--;
   short shift=(short)(size/2);
   int c,r,t,k,adr,adr1,adr2,c2,r2;
   double *valuein=(double *)malloc(samples*size*sizeof(double));
   double *empty=(double *)malloc(samples*1*sizeof(double));
   //~ double Mout,*M=(double *)malloc(samples*1*sizeof(double));
   //~ float nout,*n=(float *)malloc(samples*1*sizeof(float));
   double background=*bg;
   double f;
   short sizex,sizey;
   sizex=sizey=size;
   double *content=(double *)malloc(size*size*sizeof(double));
   if (*verbose)
      Rprintf("focalMedian: r=%d c=%d b=%d size=%d shift=%d fill=%d bg=%.1f\n"
            ,lines,samples,bands,size,shift,toFill,background);
   for (adr=0;adr<samples;adr++)
       empty[adr]=background;
   for (t=0;t<bands;t++) // get band
   {
      adr1=t*lines*samples;
      for (adr=0;adr<samples*size;adr++)
          valuein[adr]=background;
      for (r=0;r<shift+lines;r++)
      {
         //~ memset(M,0,samples*sizeof(double));
         //~ memset(n,0,samples*sizeof(float));
         memoverlap(valuein,valuein+samples,(size-1)*samples*sizeof(double));
         if (r<lines)
            memcpy(valuein+(size-1)*samples,x+adr1+r*samples,samples*sizeof(double));
         else
            memcpy(valuein+(size-1)*samples,empty,samples*sizeof(double));
         if (r<shift)
            continue;
         for (c=0;c<samples;c++)
         {
            adr2=adr1+(r-shift)*samples+c;
            res[adr2]=background;
            if (adr2>=checkadr)
            {
               Rprintf("*** ERROR *** adr2=%d(max=%d) adr1=%d t=%d r=%d c=%d\n"
                      ,adr2,checkadr,adr1,t,r,c);
               return;
            }
            if ((!toFill)&&(valuein[shift*samples+c]==background))
               continue;
            for (adr=0;adr<size*size;adr++)
               content[adr]=background;
            k=0;
            for (j=0;j<size;j++)//r
            {
               for (i=-shift;i<=shift;i++)//c
               {
                  if (c+i<0 || c+i>=samples)
                    continue;
                  if (valuein[j*samples+c+i]==background)
                     continue;
                  content[k]=valuein[j*samples+c+i];
                  k++;
               }
            }
           // if (k&&((float)k>=size*size*cover)) - find median
            if (saveMargin) {
               r2=r-shift;
               c2=c;
               if (r2<shift)
                  sizey=shift;
               else if (lines-r2-1<shift)
                  sizey=lines-r2-1;
               else
                  sizey=size;
               if (c2<shift)
                  sizex=shift;
               else if (samples-c2-1<shift)
                  sizex=samples-c2-1;
               else
                  sizex=size;
            }
            if ((float)k<sizex*sizey*cover)
               continue;
            if (k==1)
            {
               res[adr2]=content[0];
               continue;
            }
            for (i=0;i<k-1;i++)
            {
               for (j=i+1;j<k;j++)
               {
                  if (content[i]>content[j])
                  {
                     f=content[i];
                     content[i]=content[j];
                     content[j]=f;
                  }
               }
            }
            if (k%2==1)
               res[adr2]=content[k/2];
            else
               res[adr2]=0.5*(content[k/2-1]+content[k/2]);
         }
      }
   }
   return;
}
void focal4(double *x,double *bg,int *dim,int *S,int *F,double *cvr
            ,int *K,int *verbose,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   int size=*S;
   int toFill=*F;
   int kind=*K; // 0=median, 1=min, 2=max
   int checkadr=samples*lines*bands;
   float cover=(float)*cvr;
   short i,j;
   if (!(size%2))
      size--;
   short shift=(short)(size/2);
   int c,r,t,k,adr,adr1,adr2;
   double *valuein=(double *)malloc(samples*size*sizeof(double));
   double *empty=(double *)malloc(samples*1*sizeof(double));
   //~ double Mout,*M=(double *)malloc(samples*1*sizeof(double));
   //~ float nout,*n=(float *)malloc(samples*1*sizeof(float));
   double background=*bg;
   double f;
   double *content=(double *)malloc(size*size*sizeof(double));
   double minval=0.0,maxval=0.0;
   if (*verbose)
      Rprintf("focal4: r=%d c=%d b=%d kind=%d size=%d shift=%d fill=%d bg=%.1f\n"
            ,lines,samples,bands,kind,size,shift,toFill,background);
   for (adr=0;adr<samples;adr++)
       empty[adr]=background;
   for (t=0;t<bands;t++) // get band
   {
      adr1=t*lines*samples;
      for (adr=0;adr<samples*size;adr++)
          valuein[adr]=background;
      for (r=0;r<shift+lines;r++)
      {
         //~ memset(M,0,samples*sizeof(double));
         //~ memset(n,0,samples*sizeof(float));
         memoverlap(valuein,valuein+samples,(size-1)*samples*sizeof(double));
         if (r<lines)
            memcpy(valuein+(size-1)*samples,x+adr1+r*samples,samples*sizeof(double));
         else
            memcpy(valuein+(size-1)*samples,empty,samples*sizeof(double));
         if (r<shift)
            continue;
         for (c=0;c<samples;c++)
         {
            adr2=adr1+(r-shift)*samples+c;
            res[adr2]=background;
            if (adr2>=checkadr)
            {
               Rprintf("*** ERROR *** adr2=%d(max=%d) adr1=%d t=%d r=%d c=%d\n"
                      ,adr2,checkadr,adr1,t,r,c);
               return;
            }
            if ((!toFill)&&(valuein[shift*samples+c]==background))
               continue;
            for (adr=0;adr<size*size;adr++)
               content[adr]=background;
            k=0;
            for (j=0;j<size;j++)//r
            {
               for (i=-shift;i<=shift;i++)//c
               {
                  if (c+i<0 || c+i>=samples)
                    continue;
                  if (valuein[j*samples+c+i]==background)
                     continue;
                  content[k]=valuein[j*samples+c+i];
                  if (k==0)
                     minval=maxval=content[k];
                  else {
                     if (content[k]<minval)
                        minval=content[k];
                     if (content[k]>maxval)
                        maxval=content[k];
                  }
                  k++;
               }
            }
           // if (k&&((float)k>=size*size*cover)) - find median
            if ((float)k<size*size*cover)
               continue;
            if (kind==1) {
               res[adr2]=minval;
            }
            else if (kind==2) {
               res[adr2]=maxval;
            }
            else if (kind==0) {
               if (k==1)
               {
                  res[adr2]=content[0];
                  continue;
               }
               for (i=0;i<k-1;i++)
               {
                  for (j=i+1;j<k;j++)
                  {
                     if (content[i]>content[j])
                     {
                        f=content[i];
                        content[i]=content[j];
                        content[j]=f;
                     }
                  }
               }
               if (k%2==1)
                  res[adr2]=content[k/2];
               else
                  res[adr2]=0.5*(content[k/2-1]+content[k/2]);
            }
         }
      }
   }
   return;
}
void ffocal4(double *x,int *dim,double *bg,int *sz,int *fill
                ,double *cvr,int *knd,int *verb,double *res)
{
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   double background=*bg;
   double cover=*cvr;
   if (cover>1.0)
      cover=1.0;
   int kind=*knd;
   int size=*sz;
   int fillNA=*fill;
   int verbose=*verb;
   int t,r,c,r2,c2,i,j,m,adr,adr1,adr2;
   short shift=(short)(size/2);
   double eps=1e-38;
   int pb,pbmax=bands*lines;
   double minval=0.0,maxval=0.0;
   if (verbose)
      Rprintf("focal4: r=%d c=%d b=%d kind=%d size=%d shift=%d fill=%d bg=%.1f\n"
            ,lines,samples,bands,kind,size,shift,fillNA,background);
   for (t=0,pb=0;t<bands;t++) // get band
   {
      adr=t*lines*samples;
      for (r=0;r<lines;r++,pb++)
      {
         for (c=0;c<samples;c++)
         {
            adr1=adr+r*samples+c;
            m=0;
            for (j=-shift;j<=shift;j++)
            {
               r2=r+j;
               if (r2<0 || r2>=lines)
                  continue;
               for (i=-shift;i<=shift;i++)
               {
                  c2=c+i;
                  if (c2<0 || c2>=samples)
                     continue;
                  adr2=adr+r2*samples+c2;
                  if (fabs(x[adr2]-background)<eps)
                     continue;
                  if (m==0)
                     minval=maxval=x[adr2];
                  else {
                     if (x[adr2]<minval)
                        minval=x[adr2];
                     if (x[adr2]>maxval)
                        maxval=x[adr2];
                  }
                  m++;
               }
            }
            if (m<size*size*cover)
               continue;
            if (fillNA)
               res[adr1]=x[adr1];
            else
               res[adr1]=background;
           // Rprintf("%d",fabs(x[adr1]-background)<eps);
            if (((fillNA)&&(fabs(x[adr1]-background)<eps))||(!fillNA)) {
               if (kind==1) // min
                  res[adr1]=minval;
               if (kind==2) // max
                  res[adr1]=maxval;
            }
         }
         if (verbose)
            progressBar(pb,pbmax,"");
      }
   }
   return;
}
void resampl4(double *obj1,double *bg,int *dim1,int *dim2,double *lim1
             ,double *lim2,double *C,double *A,int *V,double *obj2)
{
   int debugT=-1,debugR=-1,debugC=-1;
   int verbose=*V;
   double areax,areay,area=*A;
   double cover=*C;
   double background=*bg;
   int samples1=dim1[0];
   int lines1=dim1[1];
   int bands=dim1[2];
   int samples2=dim2[0];
   int lines2=dim2[1];
   int adr,adr1,adr2;
   double minx1=lim1[0];
   double miny1=lim1[1];
   double maxx1=lim1[2];
   double maxy1=lim1[3];
   double minx2=lim2[0];
   double miny2=lim2[1];
   double maxx2=lim2[2];
   double maxy2=lim2[3];
   int resample=1;
   if (cover<0.0)
      cover=0.0;
   else if (cover>1.0)
      cover=1.0;
   if (area<0.0)
      resample=0;
   if (area<=0.0)
      area=1e-6;
   area=sqrt(area);
   int resize=1;
   if (area>1)
      resize=area;
   double resx1=(maxx1-minx1)/(float)samples1;
   double resy1=(maxy1-miny1)/(float)lines1;
   double resx2=(maxx2-minx2)/(float)samples2;
   double resy2=(maxy2-miny2)/(float)lines2;
   if (resx1>resx2)
      areax=area*resx1/resx2;
   else
      areax=area;//*resx2/resx1;
   if (resy1>resy2)
      areay=area*resy1/resy2;
   else
      areay=area;//*resy2/resy1;
   double resx3=resx2*areax;
   double resy3=resy2*areay;
   double eps=1e-11;
   int deltac=(int)(1.0+floor(resx2/(resize*resx1)));
   int deltar=(int)(1.0+floor(resy2/(resize*resy1)));
  // Rprintf("dc=%d dr=%d\n",deltac,deltar);
   if (1)
   {
      if (verbose)
         Rprintf("area=(%.2f,%.2f,%.2f) resample=%d\n",areax,areay,area,resample);
      if (verbose)
         Rprintf("dc=%d dr=%d ratio=(%f,%f)\n",deltac,deltar
                ,floor(area*resx1/resx2),floor(area*resy1/resy2));
   }
   if (verbose)
      Rprintf("resize: (%d,%d,%.3f) ==> (%d,%d,%.3f) cover=%.3f resample=%.2f bg=%.1f\n"
             ,samples1,lines1,sqrt(resx1*resy1),samples2,lines2,sqrt(resx2*resy2)
             ,cover,area,background);
   int t,c2,r2,c1,r1,c,r,n;
   double x1,y1,x2,y2;
   double W,W2,w,val,L,R,T,B;
   double val2=background;
   double w2=0.0;
   for (adr=0;adr<samples2*lines2*bands;adr++)
      obj2[adr]=background;
   for (t=0;t<bands;t++) // get band
   {
      for (r2=0;r2<lines2;r2++)
      {
         y2=miny2+(0.5+(float)(lines2-1-r2))*resy2;
         r1=(int)(lines1-1-(short)floor(lines1*(y2-miny1)/(maxy1-miny1)));
         //~ Rprintf("r2=%d r1=%d y2=%f\n",r2,r1,y2);
         if ((verbose)&&(t==debugT))
            Rprintf(" %f(%d)\n",y2,r1);
         for (c2=0;c2<samples2;c2++)
         {
            x2=minx2+(0.5+(float)c2)*resx2;
            c1=(int)floor(samples1*(x2-minx1)/(maxx1-minx1));
            adr2=t*lines2*samples2+r2*samples2+c2;
            W=W2=0.0;
            val=0.0;
            n=0;
            for (r=r1-deltar;r<=r1+deltar;r++)
            {
               if ((r<0)||(r>=lines1))
                  continue;
               y1=miny1+(0.5+(float)(lines1-1-r))*resy1;
               //~ Rprintf("   r=%d y1=%f\n",r,y1);
               //~ if (2.0*fabs(y1-y2)>(area*resy1+resy2))
                  //~ continue;
               B=(y1-resy1/2)-y2;
               if (B>resy3/2)
                  continue;
               T=(y1+resy1/2)-y2;
               if (T<-resy3/2)
                  continue;
               if (B<-resy3/2)
                  B=-resy3/2;
               if (T>resy3/2)
                  T=resy3/2;
               if (T-B==0.0)
                  continue;
               //~ Rprintf(" %.2f",(T-B)/resy2);
               for (c=c1-deltac;c<=c1+deltac;c++)
               {
                  if ((c<0)||(c>=samples1))
                     continue;
                  x1=minx1+(0.5+(float)c)*resx1;
                  //~ if (2.0*fabs(x1-x2)>(area*resx1+resx2))
                     //~ continue;
                  L=(x1-resx1/2)-x2;
                  if (L>resx3/2)
                     continue;
                  R=(x1+resx1/2)-x2;
                  if (R<-resx3/2)
                     continue;
                  if (L<-resx3/2)
                     L=-resx3/2;
                  if (R>resx3/2)
                     R=resx3/2;
                  if (R-L==0.0)
                     continue;
                  if ((verbose)&&(t==debugT)&&(r2==debugR)&&(c2==debugC))
                     Rprintf("2: dc=%d dr=%d B=%.1f T=%.1f L=%.1f R=%1.f"
                             " T-B=%.1f R-L=%.1f\n"
                            ,c-c1,r-r1,B*1e-3,T*1e-3,L*1e-3,R*1e-3
                            ,(T-B)*1e-3,(R-L)*1e-3);
                  w=(T-B)*(R-L)/(resx2*resy2*areax*areay);
                  if ((w<0)||(w>1+eps))
                     Rprintf(" %.2f\n",w);
                  //~ Rprintf(" %.2f (%f %f %f %f %f)\n"
                         //~ ,w,resx2,resy2,areax,areay,resx2*resy2*areax*areay);
                  W2+=w;
                  if ((verbose)&&(t==debugT)&&(r2==debugR)&&(c2==debugC))
                     Rprintf("w=%.2f W2=%.2f\n",w,W2);
                  adr1=t*lines1*samples1+r*samples1+c;
                  if (fabs(obj1[adr1]-background)<eps)
                     continue;
                  if ((!resample)&&(w>0.0)) {
                     if ((n==0)||(w>w2)) {
                        w2=w;
                        val2=obj1[adr1];
                     }
                     n++;
                  }
                  W+=w;
                  val+=w*obj1[adr1];
                  if (0)
                     Rprintf(" %.0f",obj1[adr1]);
                 // if (fabs(w-1.0)>1e-6)
                 //    Rprintf("%.3f\n",w);
                  if ((verbose)&&(!resample)&&(fabs(w-1.0)>1e-6)&&
                                       (fabs(w-0.5)>1e-6)&&(fabs(w-0.25)>1e-6))
                     Rprintf("only resize but w=%f (c2=%d r2=%d c=%d r=%d)\n"
                            ,w,c2,r2,c,r);
                  if ((n)&&(w2>=cover))
                     break;
               }
               if ((n)&&(w2>=cover))
                  break;
            }
            if ((W>0)&&((W/W2)>=cover)) {
               if (resample)
                  obj2[adr2]=val/W;
               else if (n) {
                 // Rprintf("w2=%f\n",w2);
                  obj2[adr2]=val2;
               }
            }
         }
      }
   }
   //~ Rprintf("\n");
   return;
}
void reclassify(double *src,int *_n,double *na,double *class,int *_nclass,int *dst)
{
   int nclass=*_nclass;
   int n=*_n;
   int i,j;
   double nodata=*na;
   for (i=0;i<n;i++)
   {
      dst[i]=nclass+1;
      if (src[i]==nodata)
         continue;
      for (j=1;j<nclass;j++)
      {
         if (src[i]<=class[j-1])
            continue;
         if (src[i]>class[j])
            continue;
         dst[i]=j-1;
      }
   }
   return;
}
void internalMargin(double *x,int *dim,int *indr,int *indc)
{
   int lines=dim[0];
   int samples=dim[1];
   int bands=dim[2];
   int c,r,s,t;
   double S;
   int l=lines*samples;
   double *res=(double *)malloc(l*sizeof(double));
  // Rprintf("dim=c(%d,%d,%d)\n",samples,lines,bands);
   for (s=0;s<l;s++)
   {
      if (bands==4)
         res[s]=(1-x[s+3*l]);
      else {
         for (S=0.0,t=0;t<bands;t++)
            S+=x[s+t*l];
         S=S/(double)bands;
         res[s]=S;
      }
     // Rprintf(" %.1f",res[s]);
   }
  // Rprintf("\n");
   for (c=0;c<samples;c++)
   {
      for (S=0.0,r=0;r<lines;r++)
         S+=res[c*lines+r];
      S=S/(double)lines;
      if (S>1-1e-6)
         indc[c]=1;
      else
         indc[c]=0;
     // Rprintf(" %d",indc[c]);
   }
  // Rprintf("\n");
   for (r=0;r<lines;r++)
   {
      for (S=0.0,c=0;c<samples;c++)
         S+=res[c*lines+r];
      S=S/(double)samples;
      if (S>1-1e-6)
         indr[r]=1;
      else
         indr[r]=0;
     // Rprintf(" %d",indr[r]);
   }
  // Rprintf("\n");
   free(res);
   return;
}
double calcAreaIncrement(double *x,int *dim,double *res,int adr0,int c, int r
                        ,int r1,int c1,int r2,int c2,int r3,int c3,int verbose) {
   int adr1,adr2,adr3;
   int samples=dim[0];
   int lines=dim[1];
   double y1,y2,y3,z0,z1,z2,z3,ret;
   double resx=0.25*res[0]*res[0];
   double resy=0.25*res[1]*res[1];
   double resz=resx+resy;
   double resw=res[0]*res[1];
   double retw=1.0/resw;
   if ((r+r1<0)||(r+r2<0)||(r+r3<0)||
       (r+r1>=lines)||(r+r2>=lines)||(r+r3>=lines)||
       (c+c1<0)||(c+c2<0)||(c+c3<0)||
       (c+c1>=samples)||(c+c2>=samples)||(c+c3>=samples))
   {
      ret=0.125;
      if (verbose)
         Rprintf(" %.3f",ret);
      return(ret);
   }
   adr1=adr0+(r+r1)*samples+(c+c1);
  // if (adr1>150000)
  //    Rprintf(" %d",adr1);
   adr2=adr0+(r+r2)*samples+(c+c2);
   adr3=adr0+(r+r3)*samples+(c+c3);
   if ((ISNA(x[adr1]))||(ISNA(x[adr2]))||(ISNA(x[adr3])))
   {
      ret=0.125;
      if (verbose)
         Rprintf(" %.3f",ret);
      return(ret);
   }
   y1=x[adr1]-x[adr2];
   y2=x[adr2]-x[adr3];
   y3=x[adr3]-x[adr1];
   z1=sqrt(resz+y1*y1);
   z2=sqrt(resx+y2*y2);
   z3=sqrt(resy+y3*y3);
   z0=0.5*(z1+z2+z3);
   ret=sqrt(z0*(z0-z1)*(z0-z2)*(z0-z3))*retw;
   if (verbose)
      Rprintf(" %.3f",ret);
   return(ret);
}
void areaIncrement(double *x,int *dim,double *res,double *out) {
   int samples=dim[0];
   int lines=dim[1];
   int bands=dim[2];
   int verbose=0;
   int c,r,t;
   int adr,adr0;
   double val;
   for (t=0;t<bands;t++) {
      adr0=t*lines*samples;
     // Rprintf("adr0=%d\n",adr0);
      for (r=0;r<lines;r++) {
         for (c=0;c<samples;c++) {
            adr=adr0+r*samples+c;
           // Rprintf(" %d",adr);
            val=calcAreaIncrement(x,dim,res,adr,c,r, 0, 0,-1,-1,-1, 0,verbose)+
                calcAreaIncrement(x,dim,res,adr,c,r, 0, 0,-1, 1,-1, 0,verbose)+
                calcAreaIncrement(x,dim,res,adr,c,r,-1, 1, 0, 0, 0, 1,verbose)+
                calcAreaIncrement(x,dim,res,adr,c,r, 1, 1, 0, 0, 0, 1,verbose)+
                calcAreaIncrement(x,dim,res,adr,c,r, 0, 0, 1, 1, 1, 0,verbose)+
                calcAreaIncrement(x,dim,res,adr,c,r, 0, 0, 1,-1, 1, 0,verbose)+
                calcAreaIncrement(x,dim,res,adr,c,r, 1,-1, 0, 0, 0,-1,verbose)+
                calcAreaIncrement(x,dim,res,adr,c,r,-1,-1, 0, 0, 0,-1,verbose);
            if (ISNA(x[adr]))
               out[adr]=NA_REAL;
            else
               out[adr]=val;
            if (verbose)
               Rprintf("\n");
         }
      }
   }
   return;
}
void groupSummary(double *x,int *dim,double *_cover,double *weight,int *_fun
                 ,double *res)
{
  /* 
     blank=0 all=1 any=2 sum=3 prod=4 min=5 max=6 range=7 mean=8 median=9 
     sd=10 var=11 length=12
  */
   int spatial=dim[0]; // samples*lines
   int temporal=dim[1]; // bands
   int c,t,adrIn,adrOut;
   double Mx,Sx,Px,w,MINx,MAXx,ALLx,ANYx,f,Vx;
   int n,i,j;
   int fun=*_fun;
   double cover=*_cover;
   if ((fun==1001)||(fun==1002)) // how to process NA in 'all', 'any'?
      cover=0.0;
  // Rprintf("dim=c(%d,%d) cover=%f fun=%d\n",spatial,temporal,cover,fun);
   double *content;
   content=(double *)malloc(temporal*sizeof(double));
   for (c=0;c<spatial;c++)
   {
      Mx=Sx=w=Vx=0.0;
      Px=1.0;
      MINx=+1e+38;
      MAXx=-1e+38;
      n=0;
      adrOut=0*spatial+c;
      adrIn=c;
      for (t=0;t<temporal;t++,adrIn+=spatial)
      {
         if (ISNA(x[adrIn]))
            continue;
         if (x[adrIn]<MINx)
            MINx=x[adrIn];
         if (x[adrIn]>MAXx)
            MAXx=x[adrIn];
         Sx+=x[adrIn];
         Px*=x[adrIn];
         Mx+=x[adrIn]*weight[t];
         w+=weight[t];
         if ((fun==9)||(fun==10))
            content[n]=x[adrIn]; //[k++]
         n++;
      }
     // Rprintf("\n");
      if (n==temporal)
         ALLx=1.0;
      else
         ALLx=0.0;
      if (n>0)
         ANYx=1.0;
      else
         ANYx=0.0;
      if ((float)n/(float)temporal<cover) {
        // Rprintf(" %f/%f=%f(%f)"
        //        ,(float)n,(float)temporal,(float)n/(float)temporal,cover);
         res[adrOut]=NA_REAL;
      }
      else {
         switch(fun)
         {
            case 0:
               res[adrOut]=0.0;
            break;
            case 1: // "all"
               res[adrOut]=ALLx;
              // res[adrOut]=(double)(n==temporal);
            break;
            case 2: // "any"
               res[adrOut]=ANYx;
              // res[adrOut]=(double)(n>=0);
            break;
            case 3: // "sum"
               res[adrOut]=Sx;
            break;
            case 4: // "prod"
               res[adrOut]=Px;
            break;
            case 5: // "min"
               res[adrOut]=MINx;
            break;
            case 6: // "max"
               res[adrOut]=MAXx;
            break;
            case 8: // "mean"
               res[adrOut]=Mx/w;
            break;
            case 9: // "median"
               if (n==1)
                  res[adrOut]=content[0];
               else
               {
                  for (i=0;i<n-1;i++)
                  {
                     for (j=i+1;j<n;j++)
                     {
                        if (content[i]>content[j])
                        {
                           f=content[i];
                          // Rprintf(" %.1f",f);
                           content[i]=content[j];
                           content[j]=f;
                        }
                     }
                  }
                 // Rprintf("\n");
                  if (n%2==1)
                     res[adrOut]=content[n/2];
                  else
                     res[adrOut]=0.5*(content[n/2-1]+content[n/2]);
               }
            break;
            case 10: // sd
               if (n==1)
                  res[adrOut]=0.0;
               else {
                  Mx=Sx/n;
                  for (i=0;i<n;i++)
                     Vx+=(content[i]-Mx)*(content[i]-Mx);
                  res[adrOut]=sqrt(Vx/(n-0)); // (n-1)
               }
            break;
            case 11: // var
               if (n==1)
                  res[adrOut]=0.0;
               else {
                  Mx=Sx/n;
                  for (i=0;i<n;i++)
                     Vx+=(content[i]-Mx)*(content[i]-Mx);
                  res[adrOut]=Vx/(n-0); // (n-1)
               }
            break;
            case 12: // "length"
               res[adrOut]=(double)n;
            break;
            default:
               res[adrOut]=0.0;
            break;
         }
      }
     // printf(" res[%2d]=%5.2f *** %5.2f ***\n",adr2,res[adr2],Mx);
   }
   free(content);
   return;
}
void dist2dist(double *x1,double *y1,double *x2,double *y2
              ,int *lenxy,int *lendf,int *pos,int *verb
              ,double *dist,int *ind)
{
   int i,j,i2;
   int n1=*lenxy;
   int n2=*lendf;
   int verbose=*verb;
   int positive=*pos;
   double d,minD;
   for (j=0;j<n2;j++)
   {
      minD=1e37;
      i2=0;
      for (i=0;i<n1;i++)
      {
         d=(x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]);
         if ((d==0.0)&&(positive))
            continue;
         if (i==0)
         {
            minD=d;
            continue;
         }
         if (d<minD)
         {
            minD=d;
            i2=i;
         }
      }
      dist[j]=sqrt(minD);
      ind[j]=i2;
      if (verbose)
         progressBar(j,n2,"");
   }
   return;
}
void isNear(double *x1,double *x2,int *len1,int *len2,int *res)
{
   int n1=*len1;
   int n2=*len2;
   int i1,i2,small;
   small=0;
   for (i1=0;i1<n1;i1++)
      if (fabs(x1[i1])<1.0)
         small++;
   if (small<2)
      small=0;
   for (i1=0;i1<n1;i1++) {
      for (i2=0;i2<n2;i2++) {
         if (small) {
            if (fabs(x1[i1]-x2[i2])<1e-27)
               break;
         }
         else {
            if (fabs(x1[i1]/x2[i2]-1.0)<1e-6)
               break;
            if ((fabs(x1[i1])<1.0)&&(fabs(x2[i2])<1.0)) {
               //~ Rprintf("near zero: %.32f\n",fabs(x1[i1]-x2[i2]));
               if (fabs(x1[i1]-x2[i2])<1e-6)
                  break;
            }
         }
      }
      //~ if (i2<n2)
         //~ Rprintf("ratio=%f\n",fabs(x1[i1]/x2[i2]-1.0));
      if (i2<n2)
         res[i1]=i2+1;
   }
   return;
}
void scatterplot(int *x,int *y,int *n,int *nbreakX,int *nbreakY
                ,int *histX,int *histY,int *hist2d) {
   int i,ndata=*n;
  // int nx=*nbreakX;
   int ny=*nbreakY;
   int c,r;
   for (i=0;i<ndata;i++) {
      c=x[i];
      r=y[i];
      histX[c]+=1;
      histY[r]+=1;
      hist2d[ny*c+r]+=1;
   }
   return;
}
int main()
{
   Rprintf("Compile and build as DLL\n");
   return (1);
}
