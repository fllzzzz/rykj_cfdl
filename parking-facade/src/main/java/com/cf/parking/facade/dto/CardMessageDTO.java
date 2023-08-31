package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @Classname LinkMessageDTO
 * @Date 2022/10/20 14:25
 * @Created by csy
 */
@Data
@Accessors(chain = true)
public class CardMessageDTO {
    /**
     * 跳转url
     */
    private String url;
    /**
     * 钉钉id
     */
    private List<String> openIdList;
    /**
     * 消息
     */
    private String message;
}
