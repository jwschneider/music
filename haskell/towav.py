#author: Pretz@github
#modified by John Schneider

import wave
import struct
import sys

def write_wav(data, filename, framerate, amplitude):
    wavfile = wave.open(filename, "w")
    nchannels = 1
    sampwidth = 2
    framerate = framerate
    nframes = len(data)
    comptype = "NONE"
    compname = "not compressed"
    wavfile.setparams((nchannels,
                        sampwidth,
                        framerate,
                        nframes,
                        comptype,
                        compname))
    frames = []
    for s in data:
        mul = int(s * amplitude)
        # print "s: %f mul: %d" % (s, mul)
        frames.append(struct.pack('h', mul))
    # frames = (struct.pack('h', int(s*self.amp)) for s in sine_list)
    frames = ''.join(frames)
    for x in xrange(0, 7200):
        wavfile.writeframes(frames)
    wavfile.close()

if __name__ == "__main__":
    data = []
    while 1:
        try:
            inp = input()
        except EOFError:
            break
        data.append(int(inp))
    write_wav(data, "test.wav", 100000, 1)
