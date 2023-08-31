package com.cf.parking.services.properties;

import com.cf.parking.facade.constant.RedisConstant;
import com.cf.support.utils.RedisUtil;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @Classname DingTalkProperties
 * @Date 2022/10/21 9:47
 * @Created by csy
 */
@Component
@ConfigurationProperties(prefix = "dingtalk")
@Data
public class DingTalkProperties {
    /**
     * keyWord
     */
    private String keyWord;
    /**
     * url
     */
    private String url;
    /**
     * agentId
     */
    private String agentId;
    /**
     * appKey
     */
    private String appKey;
    /**
     * secret
     */
    private String secret;
    /**
     * corpId
     */
    private String corpId;
    /**
     * miniAppId
     */
    private String miniAppId;
    /**
     * pVersion
     */
    private String pVersion;

    /**
     * packageType
     */
    private String packageType;
    /**
     * source
     */
    private String source;
    /**
     * version
     */
    private String version;
    /**
     * imageId
     */
    private String imageId;

    /**
     * signUrl
     */
    private String signUrl;

    /**
     * messageActionUrl
     */
    private String messageActionUrl;

    @Resource
    private RedisUtil redisUtil;


    /**
     * 获取订单详情url
     *
     * @param parkingOrderId
     * @return
     */
    public String getOrderDetailUrl(Long parkingOrderId) {

        return messageActionUrl+ parkingOrderId;
    }

}
