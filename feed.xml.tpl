<?xml version="1.0" encoding="UTF-8"?>
<rss xmlns:atom="http://www.w3.org/2005/Atom"
     xmlns:content="http://purl.org/rss/1.0/modules/content/"
     xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd"
     xmlns:media="http://search.yahoo.com/mrss/"
     xmlns:creativeCommons="http://backend.userland.com/creativeCommonsRssModule"
     version="2.0">

    <channel>
        <title>#{title}</title>
        <link>#{link}</link>
        <language>en-us</language>
        <atom:link href="#{link}/feed.xml"
                   rel="self"
                   type="application/rss+xml"/>
        <copyright>#{copyright}</copyright>
        <description>#{description}</description>
        <managingEditor>#{email}</managingEditor>
        <webMaster>#{email}</webMaster>
        <creativeCommons:license>https://creativecommons.org/licenses/by-nc-nd/4.0/</creativeCommons:license>
        <pubDate>#{pubDate}</pubDate>
        <lastBuildDate>#{latestDate}</lastBuildDate>
        <image>
            <url>#{img}</url>
            <title>#{title}</title>
            <link>#{link}</link>
        </image>
        <docs>http://www.rssboard.org/rss-specification</docs>
        <itunes:subtitle>#{itunesSubtitle}</itunes:subtitle>
        <itunes:author>#{authors}</itunes:author>
        <itunes:summary>#{itunesSummary}</itunes:summary>
        <itunes:keywords>Philosophie, Moral, Kolumbien</itunes:keywords>
        <itunes:block>yes</itunes:block>
        <itunes:owner>
            <itunes:name><![CDATA[#{authors}]]></itunes:name>
            <itunes:email>#{email}</itunes:email>
        </itunes:owner>
        <itunes:image href="#{img}" />
        <itunes:category text="Technology" />
        <itunes:explicit>no</itunes:explicit>
        %{ forall episode <- episodes }
        <item>
            <title>#{episodeTitle episode}</title>
            <link>#{link}/#{episodeSlug episode}</link>
            <description>
                #{episodeDescription episode}
            </description>
            <guid isPermaLink="false">#{link}/#{episodeSlug episode}</guid>
            <pubDate>#{episodePubdate episode}</pubDate>
            <media:content
                medium="audio"
                url="#{link}/#{episodeAudioFile episode}"
                type="#{episodeAudioContentType episode}"
                isDefault="true"
                duration="#{episodeDuration episode}">
            </media:content>
            <media:title type="plain">#{episodeTitle episode}</media:title>
            <media:description>
                #{episodeDescription episode}
            </media:description>
            <media:rating scheme="urn:simple">adult</media:rating>
            <media:keywords>moral</media:keywords>
            <enclosure url="#{link}/#{episodeAudioFile episode}" length="#{episodeFileSize episode}" type="#{episodeAudioContentType episode}" />
            %{ if (episodeThumbnailFile episode == "") }
            <media:thumbnail url="#{link}/#{defaultThumbnail}"/>
            <itunes:image href="#{link}/#{defaultThumbnail}" />
            %{ else }
            <media:thumbnail url="#{link}/#{episodeThumbnailFile episode}"/>
            <itunes:image href="#{link}/#{episodeThumbnailFile episode}" />
            %{ endif }
            <itunes:duration>00:44:26</itunes:duration>
            <itunes:explicit>yes</itunes:explicit>
        </item>
        %{ endforall }
    </channel>
</rss>
