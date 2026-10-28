// this belongs in apps/components/DP5_Workshop/tools/kde_info_properties/amigainfoextractor.cpp - Version: 1
// X-Seti - October 27 2026 - IMG Factory 1.6 - Amiga .info properties for Dolphin / KDE.
//
// Reads the DiskObject: icon type (WBTOOL etc), size, stack, default tool,
// tool types and image kinds (Classic, NewIcon, OS3.5 ICONFACE, OS4 ARGB).
// Shown in Dolphin's Properties > Details and the Information panel.

// Methods list -
// be16
// be32
// typeName
// AmigaInfoExtractor::AmigaInfoExtractor   #vers 1
// AmigaInfoExtractor::extract              #vers 1
// AmigaInfoExtractor::mimetypes            #vers 1

#include "amigainfoextractor.h"

#include <KFileMetaData/ExtractionResult>
#include <KFileMetaData/Properties>
#include <QFile>

using namespace KFileMetaData;

static quint32 be16(const QByteArray &d, int at) //vers 1
{
    return at + 2 <= d.size() ? (quint8(d[at]) << 8 | quint8(d[at + 1])) : 0;
}

static quint32 be32(const QByteArray &d, int at) //vers 1
{
    return at + 4 <= d.size() ? (be16(d, at) << 16 | be16(d, at + 2)) : 0;
}

static QString typeName(int t) //vers 1
{
    static const char *names[] = {"", "WBDISK", "WBDRAWER", "WBTOOL", "WBPROJECT",
                                  "WBGARBAGE", "WBDEVICE", "WBKICK", "WBAPPICON"};
    return (t >= 1 && t <= 8) ? QString::fromLatin1(names[t]) : QString::number(t);
}

AmigaInfoExtractor::AmigaInfoExtractor(QObject *parent) //vers 1
    : ExtractorPlugin(parent)
{
}

QStringList AmigaInfoExtractor::mimetypes() const //vers 1
{
    return {QStringLiteral("application/x-amiga-icon")};
}

void AmigaInfoExtractor::extract(ExtractionResult *result) //vers 1
{
    QFile f(result->inputUrl());
    if (!f.open(QIODevice::ReadOnly))
        return;
    const QByteArray d = f.read(1 << 20);
    if (d.size() < 78 || be16(d, 0) != 0xE310)
        return;
    result->addType(Type::Image);
    if (!(result->inputFlags() & ExtractionResult::ExtractMetaData))
        return;

    int w = be16(d, 12), h = be16(d, 14);
    const int doType = quint8(d[48]);
    const quint32 stack = be32(d, 74);
    int pos = 78 + (be32(d, 66) ? 56 : 0);                    // DrawerData
    const bool selImage = be32(d, 26) != 0;
    for (int i = 0; i < (selImage ? 2 : 1) && pos + 20 <= d.size(); ++i) {   // normal + selected Image
        const int iw = be16(d, pos + 4), ih = be16(d, pos + 6), depth = be16(d, pos + 8);
        pos += 20 + ((iw + 15) / 16) * 2 * ih * depth;
    }
    QString tool;
    if (be32(d, 50) && pos + 4 <= d.size()) {                 // DefaultTool
        const int n = be32(d, pos);
        tool = QString::fromLatin1(d.mid(pos + 4, n)).remove(QChar(0));
        pos += 4 + n;
    }
    QStringList tooltypes;
    bool newIcon = false;
    if (be32(d, 54) && pos + 4 <= d.size()) {                 // ToolTypes
        const int count = int(be32(d, pos) / 4) - 1;
        pos += 4;
        for (int i = 0; i < count && pos + 4 <= d.size(); ++i) {
            const int n = be32(d, pos);
            const QString t = QString::fromLatin1(d.mid(pos + 4, n)).remove(QChar(0));
            pos += 4 + n;
            if (t.startsWith(QLatin1String("IM1=")) || t.startsWith(QLatin1String("IM2="))) {
                if (t.startsWith(QLatin1String("IM1=")) && t.size() >= 7 && !newIcon) {
                    w = t.at(5).toLatin1() - 0x21;             // NewIcon size wins
                    h = t.at(6).toLatin1() - 0x21;
                }
                newIcon = true;
            } else if (!t.startsWith(QLatin1String("*** DON'T EDIT"))) {
                tooltypes << t;
            }
        }
    }
    QStringList kinds{QStringLiteral("Classic")};
    if (newIcon)
        kinds << QStringLiteral("NewIcon");
    const int form = d.indexOf("FORM");
    if (form >= 0 && d.mid(form + 8, 4) == "ICON") {
        kinds << QStringLiteral("OS3.5 ICONFACE");
        const int face = d.indexOf("FACE", form);
        if (face > 0 && face + 10 <= d.size()) {
            w = quint8(d[face + 8]) + 1;
            h = quint8(d[face + 9]) + 1;
        }
        if (d.indexOf("ARGB", form) > 0)
            kinds << QStringLiteral("OS4 ARGB");
    }

    result->add(Property::Width, w);
    result->add(Property::Height, h);
    result->add(Property::Description,
                QStringLiteral("%1, stack %2").arg(typeName(doType)).arg(stack));
    result->add(Property::Generator, kinds.join(QStringLiteral(", ")));
    if (!tool.isEmpty())
        result->add(Property::Subject, QStringLiteral("Default tool: %1").arg(tool));
    if (!tooltypes.isEmpty())
        result->add(Property::Comment, tooltypes.join(QStringLiteral("\n")));
}

#include "moc_amigainfoextractor.cpp"
