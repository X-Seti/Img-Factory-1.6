from PIL import Image, ImageDraw, ImageStat

def halftone(img, sample=8, scale=1, angle=45):
    channel = img.convert('L').split()[0].rotate(angle, expand=1)
    size = (channel.size[0]*scale, channel.size[1]*scale)
    bitmap = Image.new('1', size)
    draw = ImageDraw.Draw(bitmap)
    for x in range(0, channel.size[0], sample):
        for y in range(0, channel.size[0], sample):
            box = channel.crop((x, y, x+sample, y+sample))
            diameter = (ImageStat.Stat(box).mean[0]/255) ** 0.5
            edge = 0.5 * (1 - diameter)
            x_pos, y_pos = (x+edge)*scale, (y+edge)*scale
            box_edge = sample * diameter * scale
            draw.ellipse((x_pos, y_pos, x_pos+box_edge, y_pos+box_edge), fill=255)
    bitmap = bitmap.rotate(-angle, expand=1)
    return bitmap
