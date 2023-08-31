package com.cf.parking.services.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 描述  阿里云配置
 */
@Data
@Component
@ConfigurationProperties(prefix = "aliyun.access")
public class AliyunAccessProperties {
    /**
     * 阿里云access key
     */
    private String key;
    /**
     * 阿里云access secret
     */
    private String secret;
    /**
     * oss bucket
     */
    private String bucketName;
    /**
     * endPoint
     */
    private String ossEndPoint;
    /**
     * oss url
     */
    private String ossUrl;
}
