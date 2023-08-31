package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @Classname TextMessageDTO
 * @Date 2022/10/20 9:57
 * @Created by csy
 */
@Data
@Accessors(chain = true)
public class TextMessageDTO {
    /**
     * 钉钉id
     */
    private List<String> openIdList;
    /**
     * 消息
     */
    private String message;
}
