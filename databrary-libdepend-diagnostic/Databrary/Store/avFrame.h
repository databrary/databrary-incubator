#ifndef DATABRARY_MEDIA_AV_H
#define DATABRARY_MEDIA_AV_H
#include <libavformat/avformat.h>

void avFrame_initialize_stream(AVStream *o, AVFormatContext *c, AVStream *i, AVFrame *f, int w, int h);
int avFrame_rescale(const AVCodecContext *c, AVFrame *f);
#endif
