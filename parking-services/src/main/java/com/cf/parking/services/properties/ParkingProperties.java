package com.cf.parking.services.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;


/**
 * 停车场配置
 */
@Component
@ConfigurationProperties(prefix = "parking")
@Data
public class ParkingProperties {
    /**
     * 域名
     */
    private String host;
    /**
     * 新增车辆url
     */
    private String addCarmanagementUrl;
    /**
     * 查询车场url
     */
    private String queryyardUrl;
    
}
